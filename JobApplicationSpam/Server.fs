namespace JobApplicationSpam

open WebSharper
open Npgsql
open System
//open JobApplicationSpam.Types
open System.Configuration
//open Website
open System.Security.Cryptography
open System.Text.RegularExpressions
//open JobApplicationSpam.I18n
//open Variables
open System.Data
//open DBTypes
open FSharp.Data.Sql.Transactions
open System.Linq
open System.Net.Mail
open log4net
open System.Reflection
open System.IO
open WebSharper.Web.Remoting
open System.Transactions
open FSharp.Data.Sql
open System.Data.Sql
open System.Data.SqlTypes
open System.Data.SqlClient
open System.Net.Mail
open WebSharper.UI.Next.Html
open FSharp.Linq

open Types


module Server =
    open System.Linq

    let private log = LogManager.GetLogger(MethodBase.GetCurrentMethod().GetType())

    [<Literal>]
    let private connectionString = "Server=localhost; Port=5432; User Id=spam; Password=Steinmetzstr9!@#$; Database=jobapplicationspam; Enlist=true"

    [<Literal>]
    let private resolutionPath = "bin"

    type private DB =
        SqlDataProvider<
            DatabaseVendor = FSharp.Data.Sql.Common.DatabaseProviderTypes.POSTGRESQL,
            ConnectionString = connectionString,
            ResolutionPath = resolutionPath,
            IndividualsAmount = 1000,
            UseOptionTypes = true>

    [<Literal>]
    let connectionStringTest = "Server=localhost; Port=5432; User Id=spam; Password=Steinmetzstr9!@#$; Database=jobapplicationspamtest"

    let sendEmail fromAddress fromName toAddress subject body (attachmentPathsAndNames : list<string * string>) =
        use smtpClient = new SmtpClient(Settings.EmailServer, Settings.EmailPort)
        smtpClient.EnableSsl <- true
        smtpClient.Credentials <- new System.Net.NetworkCredential(Settings.EmailUsername, Settings.EmailPassword)
        let fromAddress = new MailAddress(fromAddress, fromName, System.Text.Encoding.UTF8)
        let toAddress = new MailAddress(toAddress)
        let message = new MailMessage(fromAddress, toAddress, SubjectEncoding = System.Text.Encoding.UTF8, Subject = subject, Body = body, BodyEncoding = System.Text.Encoding.UTF8)
        let attachments =
            attachmentPathsAndNames
            |> List.map (fun (filePath, fileName) ->
                let attachment = new Attachment(filePath)
                attachment.Name <- fileName
                message.Attachments.Add(attachment)
                attachment)
        smtpClient.Send(message)
        for attachment in attachments do
            attachment.Dispose()

    let toRootedPath path =
        if System.IO.Path.IsPathRooted path
        then path
        else System.IO.Path.Combine(Settings.DataDir, path)

    let withTransaction (f : DB.dataContext -> Result<'a>) =
        async {
            let dbContext = DB.GetDataContext()
            //use dbScope = new TransactionScope()
            try
                let r = f dbContext
                match r with
                | Error ->
                    dbContext.ClearUpdates() |> ignore
                    //dbScope.Dispose()
                    return Error
                | _ ->
                    dbContext.SubmitUpdates()
                    //dbScope.Complete()
                    return r
            with
            | e -> 
                log.Error e
                return Error
        }

    let writeDB (f : DB.dataContext -> Result<'a>) =
        try
            let dbContext = DB.GetDataContext()
            let r = f dbContext
            match r with
            | Error ->
                dbContext.ClearUpdates() |> ignore
                Error
            | _ ->
                dbContext.SubmitUpdates()
                r
        with
        | e -> 
            log.Error e
            Error

    let readDB f =
        try
            let dbContext = DB.GetDataContext()
            let r = f dbContext
            dbContext.ClearUpdates() |> ignore
            async { return r }
        with
        | e -> 
            log.Debug "hallo welt"
            log.Error e
            failwith "Couldn't read from database"

    let generateSalt length =
        let (bytes : array<byte>) = Array.replicate length (0uy)
        use rng = new RNGCryptoServiceProvider()
        rng.GetBytes(bytes)
        bytes |> Convert.ToBase64String     

    let generateHashWithSalt (password : string) (salt : string) iterations length =
        use deriveBytes = new Rfc2898DeriveBytes(password |> System.Text.Encoding.UTF8.GetBytes, salt |> Convert.FromBase64String, iterations)
        deriveBytes.GetBytes(length) |> Convert.ToBase64String

    let generateHash (word : string) =
        use sha256Managed = new SHA256Managed()
        let bytes = sha256Managed.ComputeHash(word |> System.Text.Encoding.UTF8.GetBytes)
        bytes |> Convert.ToBase64String

    let withCurrentUser (f : int -> 'a -> 'b) =
        let userId = GetContext().UserSession.GetLoggedInUser() |> Async.RunSynchronously
        match userId with
        | None ->
            log.Error "Nobody logged in"
            failwith "Nobody logged in"
        | Some userId ->
            match userId |> Int32.TryParse with
            | (true, v) -> f v
            | _ ->
                log.Error "UserId was not an integer"
                failwith "UserId was not an integer"

    let updateSessionGuid userId =
        let setSessionGuid sessionGuid userId (dbContext : DB.dataContext) =
            (query {
                for user in dbContext.Public.Users do
                where (user.Id = userId)
            })
            |> Seq.map (fun user -> user.Sessionguid <- Some sessionGuid)
            |> ignore
            Ok sessionGuid
        let sessionGuid = Guid.NewGuid().ToString("N")
        setSessionGuid sessionGuid userId |> withTransaction
    
    let toOption (o : Option<'a>) = 
        if o.IsSome
        then Some o.Value
        else None

    [<Remote>]
    let loginWithEmailAndPassword (email : string) (password : string) =
        try
            let usersWithCredentials (dbContext : DB.dataContext) =
                query {
                    for user in dbContext.Public.Users do
                    where (user.Email = email)
                    select ( user.Salt,
                             user.Password,
                             user.Confirmemailguid |> toOption,
                             { id = user.Id
                               gender = Gender.FromString user.Gender
                               degree = user.Degree
                               name = user.Name
                               street = user.Street
                               postcode = user.Postcode
                               city = user.City
                               email = user.Email
                               phone = user.Phone
                               mobilePhone = user.Mobilephone
                             }
                           )
                }
            match usersWithCredentials |> readDB |> Async.RunSynchronously |> Seq.toList with
            | [salt, dbPassword, confirmEmailGuid, userValues] ->
                if dbPassword = generateHashWithSalt password salt 1000 64
                then
                    GetContext().UserSession.LoginUser (userValues.id |> string) |> Async.RunSynchronously
                    async {
                        let! rSessionGuid = updateSessionGuid userValues.id
                        match confirmEmailGuid, rSessionGuid with
                        | (None, Ok sessionGuid) ->
                            return Ok (sessionGuid, LoggedInUser userValues)
                        | (Some _, Ok sessionGuid) ->
                            return Ok (sessionGuid, Guest userValues)
                        | _ -> return Error
                    }
                else async { return Failure "Email or password is wrong" }
            | [] ->
                async { return Failure "Email or password is wrong." }
            | _ ->
                async { return Error }
        with
        | e ->
            log.Error("", e)
            async { return Error }
        
    [<Remote>]
    let loginWithSessionGuid (sessionGuid : string) =
        let usersWithSessionGuid (dbContext : DB.dataContext) =
            query {
                for user in dbContext.Public.Users do
                where (user.Sessionguid.IsSome && user.Sessionguid.Value = sessionGuid)
                select { id = user.Id
                         gender = Gender.FromString user.Gender
                         degree = user.Degree
                         name = user.Name
                         street = user.Street
                         postcode = user.Postcode
                         city = user.City
                         email = user.Email
                         phone = user.Phone
                         mobilePhone = user.Mobilephone
                       }
            }
        async {
            match usersWithSessionGuid |> readDB |> Async.RunSynchronously |> Seq.toList with
            | [] -> return Failure "Session guid unknown"
            | [user] -> return Ok (LoggedInUser user)
            | _-> return Error
        }
    
    [<Remote>]
    let register (email : string) (password : string) =
        //TODO send confirmation email
        let usersWithEmail (dbContext : DB.dataContext) =
            query {
                for user in dbContext.Public.Users do
                where (user.Email = email)
            }
        match usersWithEmail |> readDB |> Async.RunSynchronously |> Seq.toList with
        | [] ->
            let createNewUser (dbContext : DB.dataContext) =
                let user = dbContext.Public.Users.Create()
                let sessionGuid = Guid.NewGuid().ToString("N")
                let confirmEmailGuid = Guid.NewGuid().ToString("N")
                let salt =  generateSalt 64
                user.Sessionguid <- Some sessionGuid
                user.Email <- email
                user.City <- "Nürnberg"
                user.Confirmemailguid <- Some confirmEmailGuid
                user.Createdon <- DateTime.Now
                user.Degree <- "Dr."
                user.Gender <- "f"
                user.Mobilephone <- "0151 2327494"
                user.Name <- "René Ederer"
                user.Password <- generateHashWithSalt password salt 1000 64
                user.Phone <- "0911 238184811"
                user.Postcode <- "90429"
                user.Salt <- salt
                user.Street <- "Raabstr. 24A"
                Ok ( sessionGuid, 
                     Guest
                       { id = user.Id
                         gender = Gender.FromString user.Gender
                         degree = user.Degree
                         name = user.Name
                         street = user.Street
                         postcode = user.Postcode
                         city = user.City
                         email = user.Email
                         phone = user.Phone
                         mobilePhone = user.Mobilephone
                       }
                   )
            createNewUser |> withTransaction
        | _ -> async { return Failure "This email is already registered" }

    let getCurrentUser () =
        let getUser userId (dbContext : DB.dataContext) =
            (query {
                for user in dbContext.Public.Users do
                where (user.Id = userId)
                select (Some user)
            }).SingleOrDefault()
        getUser |> withCurrentUser |> readDB

    [<Remote>]
    let changePassword password =
        async {
            let! oUser = getCurrentUser ()
            match  oUser with
            | Some user ->
                let setNewPasswordFor (user : DB.dataContext.``public.usersEntity``) password (dbContext : DB.dataContext) =
                    let salt = generateSalt 64
                    let hashedPassword = generateHashWithSalt password salt 1000 64
                    user.Salt <- salt
                    user.Password <- hashedPassword
                    Ok ()
                return! setNewPasswordFor user password |> withTransaction
            | None ->
                return Failure "You are not logged in."
        }

    [<Remote>]
    let changeEmail email =
        async {
            let! oUser = getCurrentUser ()
            match  oUser with
            | Some user ->
                let setNewEmailFor (user : DB.dataContext.``public.usersEntity``) email (dbContext : DB.dataContext) =
                    user.Email <- email
                    user.Confirmemailguid <- Some (Guid.NewGuid().ToString("N"))
                    user.Sessionguid <- None
                    Ok ()
                return! setNewEmailFor user email |> withTransaction
            | None ->
                return Failure "You are not logged in."
        }

    [<Remote>]
    let confirmEmail email confirmEmailGuid =
        async {
            let! oUser = getCurrentUser ()
            match oUser with
            | Some user when user.Email = email ->
                match user.Confirmemailguid with
                | None -> return Failure "Your email has already been confirmed."
                | Some guid when guid = confirmEmailGuid ->
                    let setConfirmEmailGuidToNone (user : DB.dataContext.``public.usersEntity``) (dbContext : DB.dataContext) =
                        user.Confirmemailguid <- None
                        Ok ()
                    return! setConfirmEmailGuidToNone user |> withTransaction
                | _ ->
                    return Failure "Your email could not be confirmed."
            | None ->
                return Failure "You are not logged in."
            | _ ->
                return Failure "Your email could not be confirmed."
        }

    [<Remote>]
    let getDocuments () =
        let getDocuments' (userId : int) (dbContext : DB.dataContext) =
                let documentIdsAndNames =
                    query {
                        for document in dbContext.Public.Document do
                        where (document.Userid = userId)
                        //sortBy document.Name
                        select (document.Id, document.Name)
                    }
                let documents =
                    [ for (documentId, documentName) in documentIdsAndNames do
                        let pageIds =
                            query {
                                for page in dbContext.Public.Page do
                                where (page.Documentid = documentId)
                                //sortBy page.Pageindex
                                select page.Id
                            }
                        let   filePages =
                            query {
                                for filePage in dbContext.Public.Filepage do
                                where (filePage.Id |=| pageIds)
                                select
                                    ( filePage.Pageid,
                                      FilePage
                                        { name = filePage.Name
                                          size = 0
                                          path = filePage.Path
                                        }
                                    )
                            } |> List.ofSeq
                        let htmlPages =
                            query {
                                for htmlPage in dbContext.Public.Htmlpage do
                                where (htmlPage.Id |=| pageIds)
                                select
                                    ( htmlPage.Pageid,
                                      HtmlPage
                                        { name = htmlPage.Name
                                        }
                                    )
                            } |> List.ofSeq
                        let pages = (filePages @ htmlPages) |> List.sortBy fst |> List.map snd
                        yield
                          { name = documentName
                            pages = pages
                            id = documentId
                            customVariables = ""
                            jobName = ""
                            emailSubject = ""
                            emailBody = ""
                          }
                    ] |> List.sortBy (fun x -> x.name)
                match documents with
                | [] ->
                    let dbDocument = dbContext.Public.Document.Create()
                    dbDocument.Customvariables <- ""
                    dbDocument.Deletedon <- None
                    dbDocument.Emailbody <- "Hi there!"
                    dbDocument.Emailsubject <- "Application as farmer"
                    dbDocument.Jobname <- "Farmer"
                    dbDocument.Name <- "Farmer"
                    dbDocument.Userid <- userId
                    dbContext.SubmitUpdates()
                    Ok [{ name = "Farmer"; pages = []; jobName = "Farmer"; emailSubject = "Application as farmer"; emailBody = "Hi there!"; id = dbDocument.Id; customVariables = "" }]
                | _ -> Ok documents
        getDocuments' |> withCurrentUser |> readDB
    
    [<Remote>]
    let deleteDocumentHard (document : Document) =
        let deleteDocumentHard (document : Document) (dbContext : DB.dataContext) =
            // Delete file pages
            query {
                for filePage in dbContext.Public.Filepage do
                join page in dbContext.Public.Page on (filePage.Pageid = page.Id)
                where (page.Documentid = document.id)
                select (filePage, page)
            } |> Seq.iter (fun (filePage, page) ->
                filePage.Delete()
                dbContext.SubmitUpdates()
                page.Delete()
                dbContext.SubmitUpdates()
            )

            // Delete html pages
            query {
                for htmlPage in dbContext.Public.Htmlpage do
                join page in dbContext.Public.Page on (htmlPage.Pageid = page.Id)
                where (page.Documentid = document.id)
                select (htmlPage, page)
            } |> Seq.iter (fun (htmlPage, page) ->
                htmlPage.Delete()
                dbContext.SubmitUpdates()
                page.Delete()
                dbContext.SubmitUpdates()
            )

            // Delete lastediteddocumentid
            dbContext.Public.Lastediteddocumentid.Where(fun doc -> doc.Id = document.id) |> Seq.iter(fun x -> x.Delete())
            dbContext.SubmitUpdates()

            // Delete document
            dbContext.Public.Document.Where(fun doc -> doc.Id = document.id) |> Seq.iter(fun x -> x.Delete())
            dbContext.SubmitUpdates()
            Ok ()
        deleteDocumentHard document |> withTransaction

    [<Remote>]
    let deleteDocumentSoft (document : Document) =
        let deleteDocumentSoft (document : Document) (dbContext : DB.dataContext) =
            dbContext.Public.Document.Where(fun doc -> doc.Id = document.id) |> Seq.iter(fun x -> x.Deletedon <- Some DateTime.Now)
            dbContext.SubmitUpdates()
            Ok ()
        deleteDocumentSoft document |> withTransaction

    [<Remote>]
    let saveAsNewDocument (document : Document) =
        let saveAsNew (document : Document) (dbContext : DB.dataContext) =
            let dbDocument = dbContext.Public.Document.Create()
            dbDocument.Customvariables <- document.customVariables
            dbDocument.Jobname <- document.jobName
            dbDocument.Name <- document.name
            dbDocument.Emailsubject <- document.emailSubject
            dbDocument.Emailbody <- document.emailBody
            dbContext.SubmitUpdates()

            document.pages
            |> List.iteri (fun i page ->
                let dbPage = dbContext.Public.Page.Create()
                dbPage.Documentid <- document.id
                dbPage.Pageindex <- i + 1
                dbContext.SubmitUpdates()
                match page with
                | FilePage filePage ->
                    let dbFilePage = dbContext.Public.Filepage.Create()
                    dbFilePage.Name <- filePage.name
                    dbFilePage.Path <- filePage.path
                    dbFilePage.Pageid <- dbPage.Id
                | HtmlPage htmlPage ->
                    let dbHtmlPage = dbContext.Public.Htmlpage.Create()
                    dbHtmlPage.Name <- htmlPage.name
                    dbHtmlPage.Pageid <- dbPage.Id
                dbContext.SubmitUpdates()
                )
            Ok ()
        ()
    
    [<Remote>]
    let addFilePage fileName filePath (documentId : int) =
        let addFilePage' (dbContext : DB.dataContext) =
            let pageIndex =
                (query {
                    for page in dbContext.Public.Page do
                    where (page.Documentid = documentId)
                    sortByDescending (page.Pageindex)
                    select (Some page.Pageindex)
                }).FirstOrDefault() |> Option.map ((+) 1) |> Option.defaultValue 1

            let dbPage = dbContext.Public.Page.Create()
            dbPage.Documentid <- documentId
            dbPage.Pageindex <- pageIndex
            dbContext.SubmitUpdates()

            let filePage = dbContext.Public.Filepage.Create()
            filePage.Pageid <- dbPage.Id
            filePage.Name <- fileName
            filePage.Path <- filePath
            dbContext.SubmitUpdates()
            Ok ()
        addFilePage' |> withTransaction

    [<Remote>]
    let getFilePageNames documentId =
        let getFilePageNames documentId (dbContext : DB.dataContext) =
            query {
                for filePage in dbContext.Public.Filepage do
                join page in dbContext.Public.Page on (filePage.Pageid = page.Id)
                where (page.Documentid = documentId)
                select filePage.Name
            } |> Seq.distinct |> Seq.toList
        getFilePageNames documentId |> readDB
    
    [<Remote>]
    let getReplaceValuesMap (employer : Employer) (userValues : UserValues) (document : Document) =
        async {
            let predefinedVariables =
                [ ("$firma", employer.company)
                  ("$firmaStrasse", employer.street)
                  ("$firmaPlz", employer.postcode)
                  ("$firmaStadt",employer.city)
                  ("$chefGeschlecht", employer.gender.ToString())
                  ("$chefTitel", employer.degree)
                  ("$chefVorname", employer.firstName)
                  ("$chefNachname", employer.lastName)
                  ("$chefEmail", employer.email)
                  ("$chefTelefon", employer.phone)
                  ("$chefMobil", employer.mobilePhone)

                  ("$meinGeschlecht", userValues.gender.ToString())
                  ("$meinTitel", userValues.degree)
                  ("$meinName", "ÜtherÄÜ")
                  ("$meinVorname", "ÜtherÄÜ")
                  ("$meinName", "ÜtherÄÜ")
                  ("$meineStrasse", userValues.street)
                  ("$meinePlz", userValues.postcode)
                  ("$meinePostleitzahl", userValues.postcode)
                  ("$meineStadt", userValues.city)
                  ("$meineEmail", userValues.email)
                  ("$meineTelefonnr", userValues.phone)
                  ("$meineMobilnr", userValues.mobilePhone)
                  ("$tagHeute", sprintf "%02i" DateTime.Today.Day)
                  ("$monatHeute", sprintf "%02i" DateTime.Today.Month)
                  ("$jahrHeute", sprintf "%04i" DateTime.Today.Year)
                  ("$datumHeute", sprintf "%02i.%02i.%04i" DateTime.Today.Day DateTime.Today.Month DateTime.Today.Year)
                  ("$beruf", document.jobName)
                ]
            let customVariables = []
                //match parse customVariablesStr with
                //| Bad xs -> failwith (String.Concat xs)
                //| Ok (parsedVariables, _) ->
                //    parsedVariables
                //    |> List.map (fun (k : AssignedVariable, v : Variables.Expression) -> 
                //              (k
                //            , (tryGetValue v predefinedVariables |> Option.defaultValue ""))
                //        )

            return (predefinedVariables @ customVariables) |> List.sortByDescending (fun (k, v) -> k.Length)
        }

    [<Remote>]
    let replaceVariables
            (filePath : string)
            (userValues : UserValues)
            (employer : Employer)
            (document : Document) =
            async {
                try
                    let tmpDirectory = Path.Combine(Settings.DataDir, "tmp", Guid.NewGuid().ToString("N"))
                    let! replaceValuesMap = getReplaceValuesMap employer userValues document
                    Directory.CreateDirectory(tmpDirectory) |> ignore
                    if filePath.ToLower().EndsWith(".odt")
                    then
                        return
                            FileConverter.replaceInOdt
                                (toRootedPath filePath)
                                (Path.Combine(tmpDirectory, "extracted"))
                                (Path.Combine(tmpDirectory, "replaced"))
                                replaceValuesMap
                    else
                        let newFilePath = Path.Combine(tmpDirectory, (Path.GetFileName(filePath)))
                        File.Copy(toRootedPath filePath, newFilePath, true)
                        FileConverter.replaceInFile newFilePath replaceValuesMap
                        return newFilePath
                with
                | e ->
                    log.Error ("", e)
                    return failwith "An error occurred"
            }
    
    [<Remote>]
    let applyNow (userValues : UserValues) (employer : Employer) (document : Document) =
        let applyNow' (dbContext : DB.dataContext) =
            //set userValues
            let oldUser =
                (query {
                    for user in dbContext.Public.Users do
                    where (user.Id = userValues.id)
                }).Single()
            let newUser = dbContext.Public.Users.Create()
            newUser.City <- userValues.city
            newUser.Confirmemailguid <- oldUser.Confirmemailguid
            newUser.Createdon <- oldUser.Createdon
            newUser.Degree <- userValues.degree
            newUser.Deletedon <- None
            newUser.Email <- userValues.email
            newUser.Gender <- userValues.gender.ToString()
            newUser.Mobilephone <- userValues.mobilePhone
            newUser.Phone <- userValues.phone
            newUser.Postcode <- userValues.postcode
            newUser.Name <- userValues.name
            newUser.Password <- oldUser.Password
            newUser.Salt <- oldUser.Salt
            newUser.Sessionguid <- None //TODO
            newUser.Street <- userValues.street

            oldUser.Deletedon <- Some DateTime.Now
            dbContext.SubmitUpdates()

            //add employer
            let dbEmployer = dbContext.Public.Employer.Create()
            dbEmployer.Company <- employer.company
            dbEmployer.City <- employer.city
            dbEmployer.Degree <- employer.degree
            dbEmployer.Email <- employer.email
            dbEmployer.Firstname <- employer.firstName
            dbEmployer.Gender <- employer.gender.ToString()
            dbEmployer.Mobilephone <- employer.mobilePhone
            dbEmployer.Phone <- employer.phone
            dbEmployer.Postcode <- employer.postcode
            dbEmployer.Lastname <- employer.lastName
            dbEmployer.Street <- employer.street
            dbContext.SubmitUpdates()

            //add document
            let oldDocument =
                (query {
                    for doc in dbContext.Public.Document do
                    where (doc.Id = document.id)
                }).Single()
            let dbDocument = dbContext.Public.Document.Create()
            dbDocument.Customvariables <- document.customVariables
            dbDocument.Deletedon <- None
            dbDocument.Emailbody <- document.emailBody
            dbDocument.Emailsubject <- document.emailSubject
            dbDocument.Jobname <- document.jobName
            dbDocument.Name <- document.name
            dbDocument.Userid <- newUser.Id

            oldDocument.Deletedon <- Some DateTime.Now
            dbContext.SubmitUpdates()

            //add pages
            document.pages
            |> List.iteri (fun pageIndex page ->
                let dbPage = dbContext.Public.Page.Create()
                dbPage.Documentid <- dbDocument.Id
                dbPage.Pageindex <- pageIndex
                dbContext.SubmitUpdates()

                match page with
                | FilePage filePage ->
                    let dbFilePage = dbContext.Public.Filepage.Create()
                    dbFilePage.Name <- filePage.name
                    dbFilePage.Pageid <- dbPage.Id
                    dbFilePage.Path <- filePage.path
                | HtmlPage htmlPage ->
                    let dbHtmlPage = dbContext.Public.Htmlpage.Create()
                    dbHtmlPage.Name <- htmlPage.name
                    dbHtmlPage.Pageid <- dbPage.Id
                dbContext.SubmitUpdates()
            )

            // insert application
            let dbApplication = dbContext.Public.Application.Create()
            dbApplication.Documentid <- dbDocument.Id
            dbApplication.Userid <- newUser.Id
            dbApplication.Employerid <- dbEmployer.Id
            dbContext.SubmitUpdates()

            //insert sentStatus
            let dbSentStatus = dbContext.Public.Sentstatus.Create()
            dbSentStatus.Applicationid <- dbApplication.Id
            dbSentStatus.Dueon <- None
            dbSentStatus.Sentstatusvalueid <- 1
            dbSentStatus.Statuschangedon <- DateTime.Now
            dbSentStatus.Statusmessage <- ""
            dbContext.SubmitUpdates()

            let (|PdfPage|OdtPage|HtmlPage|Unknown|) (page : Page) =
                match page with
                | HtmlPage htmlPage -> HtmlPage htmlPage
                | FilePage filePage ->
                    match Path.getExtensionNoDot(filePage.path).ToLower() with
                    | "odt" -> OdtPage filePage
                    | "pdf" -> PdfPage filePage
                    | s -> Unknown s


            // with pages do replace values and convert to pdf
            let pdfFilePaths =
                [ for page in document.pages do
                    match page with
                    | OdtPage filePage ->
                        yield
                            replaceVariables
                                filePage.path
                                userValues
                                employer
                                document
                            |> Async.RunSynchronously
                            |> FileConverter.convertToPdf
                            |> toRootedPath
                    | PdfPage filePage ->
                        yield filePage.path |> toRootedPath
                    | HtmlPage htmlPage ->
                        yield ""
                    | Unknown s ->
                        failwith <| "Unknown file type: " + s
                ]
            let mergedPdfFilePath = Path.Combine(Settings.DataDir, "tmp", Guid.NewGuid().ToString("N") + ".pdf")
            if pdfFilePaths <> [] then FileConverter.mergePdfs pdfFilePaths mergedPdfFilePath
            sendEmail userValues.email userValues.name userValues.email document.emailSubject document.emailBody [mergedPdfFilePath, "Bewerbung"]
            Ok ()
        applyNow' |> withTransaction
         
    //[<Remote>]
    //let createLink filePath name =
    //    async {
    //        let linkGuid = Guid.NewGuid().ToString("N")
    //        return 
    //            Database.insertLink filePath name linkGuid
    //            |> withTransaction
    //            |> (fun _ -> linkGuid)
    //    }

    //[<Remote>]
    //let tryGetPathAndNameByLinkGuid linkGuid =
    //    async {
    //        return Database.tryGetPathAndNameByLinkGuid linkGuid |> readDB
    //    }

    //[<Remote>]
    //let deleteLink linkGuid =
    //    async {
    //        Database.deleteLink linkGuid |> withTransaction
    //    }

    //[<Remote>]
    //let getSentApplications () =
    //    let userId = withCurrentUser ()
    //    async {
    //        return
    //            userId
    //            |> Database.getSentApplications
    //            |> readDB
    //    }

    //let getFilesWithExtension extension userId =
    //    async {
    //        return Database.getFilesWithExtension extension userId |> readDB
    //    }

    //[<Remote>]
    //let getNotYetSentApplicationIds () = 
    //    async {
    //        return Database.getNotYetSentApplicationIds |> readDB
    //    }

    //let getFilePageNames (documentId : DocumentId) =
    //    async {
    //        return Database.getFilePageNames documentId |> readDB
    //    }

    //[<Remote>]
    //let tryFindSentApplication (employer : Employer) =
    //    let userId = withCurrentUser ()
    //    async {
    //        return
    //            userId
    //            |> Database.tryGetSentApplication employer
    //            |> readDB
    //    }

    
