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

    let log = LogManager.GetLogger(MethodBase.GetCurrentMethod().GetType())

    [<Literal>]
    let private connectionString = "Server=localhost; Port=5432; User Id=spam; Password=Steinmetzstr9!@#$; Database=jobapplicationspam; Enlist=true"

    [<Literal>]
    let private resolutionPath = "bin"

    type DB =
        SqlDataProvider<
            DatabaseVendor = FSharp.Data.Sql.Common.DatabaseProviderTypes.POSTGRESQL,
            ConnectionString = connectionString,
            ResolutionPath = resolutionPath,
            IndividualsAmount = 1000,
            UseOptionTypes = true>

    [<Literal>]
    let connectionStringTest = "Server=localhost; Port=5432; User Id=spam; Password=Steinmetzstr9!@#$; Database=jobapplicationspamtest"

    let withTransaction (f : DB.dataContext -> Result<'a>) =
        log.Debug "withTransaction"
        async {
            let dbContext = DB.GetDataContext()
            use dbScope = new TransactionScope()
            try
                let r = f dbContext
                match r with
                | Error ->
                    dbContext.ClearUpdates() |> ignore
                    dbScope.Dispose()
                    return Error
                | _ ->
                    dbContext.SubmitUpdates()
                    dbScope.Complete()
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
            failwith "Nobody logged in"
        | Some userId ->
            match userId |> Int32.TryParse with
            | (true, v) -> f v
            | _ -> failwith "UserId was not an integer"
    
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
        let usersWithCredentials (dbContext : DB.dataContext) =
            query {
                for user in dbContext.Public.Users do
                where (user.Email.IsSome && user.Email.Value = email)
                select ( user.Id,
                         user.Salt,
                         user.Password,
                         user.Confirmemailguid |> toOption,
                         { gender = Gender.FromString user.Gender
                           degree = user.Degree
                           name = user.Name
                           street = user.Street
                           postcode = user.Postcode
                           city = user.City
                           email = user.Email |> Option.defaultValue ""
                           phone = user.Phone
                           mobilePhone = user.Mobilephone
                         }
                       )
            }
        match usersWithCredentials |> readDB |> Async.RunSynchronously |> Seq.toList with
        | [userId, salt, dbPassword, confirmEmailGuid, userValues] ->
            if dbPassword = generateHashWithSalt password salt 1000 64
            then
                GetContext().UserSession.LoginUser (userId |> string) |> Async.RunSynchronously
                async {
                    let! rSessionGuid = updateSessionGuid userId
                    return Ok ("a", Guest emptyUserValues)
                    //match confirmEmailGuid, rSessionGuid with
                    //| (None, Ok sessionGuid) ->
                    //    return Ok (sessionGuid, LoggedInUser userValues)
                    //| (Some _, Ok sessionGuid) ->
                    //    return Ok (sessionGuid, Guest userValues)
                    //| _ -> return Error
                }
            else async { return Failure "Email or password is wrong" }
        | [] ->
            async { return Failure "Email or password is wrong." }
        | _ ->
            async { return Error }
        
    [<Remote>]
    let loginWithSessionGuid (sessionGuid : string) =
        let usersWithSessionGuid (dbContext : DB.dataContext) =
            query {
                for user in dbContext.Public.Users do
                where (user.Sessionguid.IsSome && user.Sessionguid.Value = sessionGuid)
                select { gender = Gender.FromString user.Gender
                         degree = user.Degree
                         name = user.Name
                         street = user.Street
                         postcode = user.Postcode
                         city = user.City
                         email = user.Email |> Option.defaultValue ""
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
                where (user.Email.IsSome && user.Email.Value = email)
            }
        match usersWithEmail |> readDB |> Async.RunSynchronously |> Seq.toList with
        | [] ->
            let createNewUser (dbContext : DB.dataContext) =
                let user = dbContext.Public.Users.Create()
                let sessionGuid = Guid.NewGuid().ToString("N")
                let confirmEmailGuid = Guid.NewGuid().ToString("N")
                let salt =  generateSalt 64
                user.Sessionguid <- Some sessionGuid
                user.Email <- Some email
                user.City <- ""
                user.Confirmemailguid <- Some confirmEmailGuid
                user.Createdon <- DateTime.Now
                user.Degree <- ""
                user.Gender <- "u"
                user.Mobilephone <- ""
                user.Name <- ""
                user.Password <- generateHashWithSalt password salt 1000 64
                user.Phone <- ""
                user.Postcode <- ""
                user.Salt <- salt
                user.Street <- ""
                Ok ( sessionGuid, 
                     Guest
                       { gender = Gender.FromString user.Gender
                         degree = user.Degree
                         name = user.Name
                         street = user.Street
                         postcode = user.Postcode
                         city = user.City
                         email = user.Email |> Option.defaultValue ""
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
        let getDocuments1 (userId : int) (dbContext : DB.dataContext) =
                let documentIdsAndNames =
                    query {
                        for document in dbContext.Public.Document do
                        where (document.Userid = userId)
                        //sortBy document.Name
                        select (document.Id, document.Name)
                    }
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
                ] |> List.sortBy (fun x -> x.name) |> Ok
        getDocuments1 |> withCurrentUser |> readDB
    
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

    //[<Remote>]
    //let saveNewDocument (document : Document) =
    //    let userId = withCurrentUser ()
    //    async {
    //        return
    //            userId
    //            |> Database.insertDocument document
    //            |> withTransaction
    //            }

    //[<Remote>]
    //let deleteDocument (DocumentId documentId) =
    //    (fun dbContext ->
    //        async {
    //            return Database.deleteDocument documentId dbContext
    //            let filePaths = Database.getDeletableFilePaths documentId dbContext
    //            for filePath in filePaths do
    //                if Path.IsPathRooted filePath
    //                then File.Delete filePath
    //                else File.Delete <| toRootedPath filePath
    //            Database.deleteDeletableDocumentFilePages documentId dbContext
    //        }
    //    ) |> withTransaction

    //[<Remote>]
    //let getDocumentOffset (htmlJobApplicationOffset : int) =
    //    let userId = withCurrentUser ()
    //    async {
    //        return
    //            userId
    //            |> Database.getDocumentOffset htmlJobApplicationOffset
    //            |> readDB
    //            }

    //[<Remote>]
    //let replaceVariables
    //        (filePath : string)
    //        (userValues : UserValues)
    //        (employer : Employer)
    //        (document : Document) =
    //    let oUserEmail = tryGetUserEmail () |> Async.RunSynchronously
    //    match oUserEmail with
    //    | Some userEmail ->
    //        async {
    //            try
    //                let tmpDirectory = Path.Combine(Settings.DataDirectory, "tmp", Guid.NewGuid().ToString("N"))
    //                let! map = toCV employer userValues userEmail document.jobName document.customVariables
    //                Directory.CreateDirectory(tmpDirectory) |> ignore
    //                if filePath.ToLower().EndsWith(".odt")
    //                then
    //                    return
    //                        Odt.replaceInOdt
    //                            (toRootedPath filePath)
    //                            (Path.Combine(tmpDirectory, "extracted"))
    //                            (Path.Combine(tmpDirectory, "replaced"))
    //                            map
    //                elif unoconvImageTypes |> List.contains (Path.GetExtension(filePath).ToLower().Substring(1))
    //                then
    //                    return filePath
    //                else
    //                    let newFilePath = Path.Combine(tmpDirectory, (Path.GetFileName(filePath)))
    //                    File.Copy(toRootedPath filePath, newFilePath, true)
    //                    Odt.replaceInFile newFilePath map Types.Ignore
    //                    return newFilePath
    //            with
    //            | e ->
    //                log.Error ("", e)
    //                return failwith "An error occurred"
    //        }
    //    | None ->
    //        log.Error("User email was None")
    //        failwith "User email was None"
         
    //let emailSentApplicationToUser' (sentApplicationOffset : int) (customVariablesString : string) userId =
    //    try
    //        let oUserEmail = Database.getEmailByUserId userId |> readDB
    //        let oSentApplication =
    //            withCurrentUser ()
    //            |> Database.getSentApplicationOffset sentApplicationOffset
    //            |> readDB
    //        match oUserEmail, oSentApplication with
    //        | _, None -> fail "The requested application could not be not found"
    //        | None, _ -> fail "User email was None"
    //        | Some userEmail, Some sentApplication ->
    //            let myList =
    //                toCV
    //                    sentApplication.employer
    //                    sentApplication.user.values
    //                    sentApplication.user.email
    //                    sentApplication.jobName
    //                    customVariablesString
    //                |> Async.RunSynchronously
    //            let tmpDirectory = Path.Combine(Settings.DataDirectory, "tmp", Guid.NewGuid().ToString("N"))
    //            let odtPaths =
    //                [ for (path, _) in sentApplication.filePages do
    //                        yield
    //                            if unoconvImageTypes |> List.contains(Path.GetExtension(path).Substring(1).ToLower())
    //                            then toRootedPath path
    //                            elif path.ToLower().EndsWith(".odt")
    //                            then
    //                                let directoryGuid = Guid.NewGuid().ToString("N")
    //                                Odt.replaceInOdt
    //                                    (toRootedPath path)
    //                                    (Path.Combine(tmpDirectory, directoryGuid, "extractedOdt"))
    //                                    (Path.Combine(tmpDirectory, directoryGuid, "replacedOdt"))
    //                                    myList
    //                            else
    //                                let copiedPath = Path.Combine(tmpDirectory, Guid.NewGuid().ToString("N"), Path.GetFileName(path))
    //                                Directory.CreateDirectory(Path.GetDirectoryName copiedPath) |> ignore
    //                                File.Copy(toRootedPath path, copiedPath)
    //                                Odt.replaceInFile
    //                                    copiedPath
    //                                    myList
    //                                    Types.Ignore
    //                                copiedPath
    //                ]

    //            let pdfPaths =
    //                [ for odtPath in odtPaths do
    //                    yield Odt.odtToPdf odtPath
    //                ] |> List.choose id
    //            let mergedPdfPath =
    //                Path.Combine(
    //                    tmpDirectory,
    //                    (sprintf
    //                        "Bewerbung_%s_%s.pdf"
    //                        sentApplication.user.values.firstName
    //                        sentApplication.user.values.lastName
    //                    ).Replace("_.", ".").Replace("_.", "."))
    //            if pdfPaths <> [] then Odt.mergePdfs pdfPaths mergedPdfPath
    //            sendEmail
    //                Settings.EmailUsername
    //                "www.bewerbungsspam.de"
    //                userEmail
    //                (Odt.replaceInString sentApplication.email.subject myList Types.Ignore)
    //                (Odt.replaceInString (sentApplication.email.body.Replace("\\r\\n", "\n").Replace("\\n", "\n")) myList Types.Ignore)
    //                (if pdfPaths = []
    //                 then []
    //                 else [mergedPdfPath,
    //                       (sprintf
    //                            "Bewerbung_%s_%s.pdf"
    //                            sentApplication.user.values.firstName
    //                            sentApplication.user.values.lastName
    //                       ).Replace("_.", ".").Replace("_.", ".")]
    //                 )
    //            ok ()
    //    with
    //    | e ->
    //        log.Error ("", e)
    //        fail "Couldn't email the application to the user"

    //[<Remote>]
    //let emailSentApplicationToUser (sentApplicationOffset : int) (customVariablesString : string) =
    //    let userId = withCurrentUser ()
    //    async {
    //        return
    //            userId
    //            |> fun userId () -> emailSentApplicationToUser' sentApplicationOffset customVariablesString userId
    //            |> fun f -> f()
    //    }



    //[<Remote>]
    //let sendNotYetSentApplication' sentApplicationId dbContext =
    //    let oSentApp = Database.getSentApplication sentApplicationId dbContext
    //    match oSentApp with
    //    | None ->
    //        log.Error (sprintf "sentApplication was None. Id: %i" sentApplicationId)
    //    | Some sentApp ->
    //        let myList =
    //                toCV
    //                    sentApp.employer
    //                    sentApp.user.values
    //                    sentApp.user.email
    //                    sentApp.jobName
    //                    sentApp.customVariables
    //                |> Async.RunSynchronously
    //        let tmpDirectory = Path.Combine(Settings.DataDirectory, "tmp", Guid.NewGuid().ToString("N"))
    //        let odtPaths =
    //            [ for (filePath, pageIndex) in sentApp.filePages do
    //                yield
    //                    if unoconvImageTypes |> List.contains(Path.GetExtension(filePath).ToLower().Substring(1))
    //                    then
    //                        toRootedPath filePath
    //                    elif filePath.ToLower().EndsWith(".odt")
    //                    then
    //                        let directoryGuid = Guid.NewGuid().ToString("N")
    //                        Odt.replaceInOdt
    //                            (toRootedPath filePath)
    //                            (Path.Combine(tmpDirectory, directoryGuid, "extractedOdt"))
    //                            (Path.Combine(tmpDirectory, directoryGuid, "replacedOdt"))
    //                            myList
    //                    else
    //                        let copiedPath = Path.Combine(tmpDirectory, Guid.NewGuid().ToString("N"), Path.GetFileName(filePath))
    //                        Directory.CreateDirectory(Path.GetDirectoryName copiedPath) |> ignore
    //                        File.Copy(toRootedPath filePath, copiedPath)
    //                        Odt.replaceInFile
    //                            copiedPath
    //                            myList
    //                            Types.Ignore
    //                        copiedPath
    //            ]
                   

    //        let pdfPaths =
    //            [ for odtPath in odtPaths do
    //                yield
    //                    Odt.odtToPdf odtPath
    //            ] |> List.choose id
    //        let mergedPdfPath = Path.Combine(tmpDirectory, Guid.NewGuid().ToString() + ".pdf")
    //        if pdfPaths <> []
    //        then Odt.mergePdfs pdfPaths mergedPdfPath


    //        //TODO
    //        dbContext.Public.Sentstatus.Create
    //            ( Sentapplicationid = sentApplicationId
    //            , Statuschangedon = DateTime.Today
    //            , Dueon = None
    //            , Sentstatusvalueid = 2
    //            , Statusmessage = "")
    //        |> ignore

    //        sendEmail
    //            sentApp.user.email
    //            (sentApp.user.values.firstName + " " + sentApp.user.values.lastName)
    //            sentApp.employer.email
    //            (Odt.replaceInString sentApp.email.subject myList Types.Ignore)
    //            (Odt.replaceInString (sentApp.email.body.Replace("\\r\\n", "\n").Replace("\\n", "\n")) myList Types.Ignore)

    //            (if pdfPaths = []
    //             then []
    //             else [mergedPdfPath, (sprintf "Bewerbung_%s_%s.pdf" sentApp.user.values.firstName sentApp.user.values.lastName)]
    //             )
    //        sendEmail
    //            sentApp.user.email
    //            (sentApp.user.values.firstName + " " + sentApp.user.values.lastName)
    //            sentApp.user.email
    //            ("Deine Bewerbung wurde versandt - " + Odt.replaceInString sentApp.email.subject myList Types.Ignore)
    //            ((sprintf
    //                "Deine Bewerbung wurde am %s an %s versandt.\n\n"
    //                (DateTime.Now.ToShortDateString())
    //                sentApp.employer.email)
    //                    + Odt.replaceInString (sentApp.email.body.Replace("\\r\\n", "\n").Replace("\\n", "\n")) myList Types.Ignore)

    //            (if pdfPaths = []
    //             then []
    //             else [mergedPdfPath, (sprintf "Bewerbung_%s_%s.pdf" sentApp.user.values.firstName sentApp.user.values.lastName)]
    //             )

    //        let oConfirmEmailGuid = Database.tryGetConfirmEmailGuid sentApp.user.email dbContext
    //        match oConfirmEmailGuid with
    //        | Some confirmEmailGuid ->
    //            sendEmail
    //                Settings.EmailUsername
    //                "Bewerbungsspam"
    //                sentApp.user.email
    //                (t German PleaseConfirmYourEmailAddressEmailSubject)
    //                (String.Format((t German PleaseConfirmYourEmailAddressEmailBody), sentApp.user.email, confirmEmailGuid))
    //                []
    //        | None -> ()
    //[<Remote>]
    //let sendNotYetSentApplication sentApplicationId =
    //    sendNotYetSentApplication' sentApplicationId
    //    |> withTransaction

    //[<Remote>]
    //let applyNow
    //        (employer : Employer)
    //        (document : Document)
    //        (userValues : UserValues)
    //        (url : string) =
    //    let userId = withCurrentUser()
    //    async {
    //        return
    //            userId
    //            |>
    //            fun userId dbContext ->
    //                Database.setUserValues userValues userId dbContext |> ignore
    //                dbContext.SubmitUpdates()
    //                let oUserEmail = Database.getEmailByUserId userId dbContext
    //                match oUserEmail with
    //                | None ->
    //                    log.Error("UserEmail was None")
    //                    fail "User email was None"
    //                | Some userEmail ->
    //                    let dbEmployer = Database.insertEmployer employer userId dbContext
    //                    dbContext.SubmitUpdates()
    //                    Database.insertNotYetSentApplication
    //                        dbEmployer.Id
    //                        document.email
    //                        (userEmail, userValues)
    //                        (document.pages |> List.choose(fun x -> match x with FilePage p -> Some (p.path, p.pageIndex) | HtmlPage _ -> None))
    //                        document.jobName
    //                        url
    //                        document.customVariables
    //                        DateTime.Today
    //                        userId
    //                        dbContext
    //                    dbContext.SubmitUpdates()
    //                    ok ()
    //            |> withTransaction
    //    }

    //[<Remote>]
    //let addFilePage (DocumentId documentId) path pageIndex name =
    //    async {
    //        return
    //            Database.insertFilePage documentId path pageIndex name |> withTransaction
    //    }

    
    //[<Remote>]
    //let getHtmlPageTemplates () =
    //    async {
    //        return Database.getHtmlPageTemplates |> readDB
    //    }
    
    //[<Remote>]
    //let getHtmlPageTemplate (templateId : int) =
    //    async {
    //        return Database.getHtmlPageTemplate templateId |> readDB
    //    }
    
    //[<Remote>]
    //let getHtmlPages documentId =
    //    async {
    //        Database.getHtmlPages documentId |> readDB
    //    }

    //[<Remote>]
    //let getPageMapOffset (pageIndex : int) (documentIndex : int)  =
    //    let userId = withCurrentUser  ()
    //    async {
    //        return
    //            userId
    //            |> Database.getPageMapOffset pageIndex documentIndex
    //            |> readDB
    //    }
     
    //[<Remote>]
    //let getLastEditedDocumentOffset () =
    //    let userId = withCurrentUser ()
    //    async {
    //        return
    //            userId
    //            |> Database.getLastEditedDocumentOffset
    //            |> readDB
    //    }

    //[<Remote>]
    //let setLastEditedDocumentId (userId : UserId) (documentId : DocumentId) =
    //    async {
    //        return Database.setLastEditedDocumentId documentId userId |> withTransaction
    //    }

    //[<Remote>]
    //let addNewDocument name =
    //    let userId = withCurrentUser  ()
    //    async {
    //        return
    //            userId
    //            |> Database.insertDocument name
    //            |> withTransaction
    //        }

    //[<Remote>]
    //let addHtmlPage (documentId : int) (oTemplateId : option<int>) (pageIndex : int) (name : string) =
    //    async {
    //        Database.insertHtmlPage documentId oTemplateId pageIndex name |> withTransaction
    //    }

    //[<Remote>]
    //let getDocumentIdOffset documentIndex =
    //    let userId = withCurrentUser ()
    //    async {
    //        return
    //            userId
    //            |> Database.tryGetDocumentIdOffset documentIndex
    //            |> readDB
    //    }
    
    //[<Remote>]
    //let readWebsite (identifier : string) : Async<Result<Employer, string>> =
    //    async {
    //        return Website.read identifier
    //    }
    
    //[<Remote>]
    //let isLoggedIn() =
    //    let loggedIn = getCurrentUserId() |> Async.RunSynchronously |> Option.isSome
    //    async { return loggedIn }
    
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

    
