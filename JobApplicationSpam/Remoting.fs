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
//open WebSharper.Owin.EnvKey.WebSharper
open Types


module Server =
    type Result<'a> =
    | Ok of 'a
    | Failure of string
    | Warning of string
    | Error of string

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
    let private connectionStringTest = "Server=localhost; Port=5432; User Id=spam; Password=Steinmetzstr9!@#$; Database=jobapplicationspamtest"
    let private defaultContext = DB.GetDataContext(connectionString)

    let withTransaction (f : DB.dataContext -> Async<Result<'a>>) =
        async {
            try
                use dbScope = new TransactionScope()
                let dbContext = DB.GetDataContext()
                let! r = f dbContext
                match r with
                | Error msg ->
                    dbContext.ClearUpdates() |> ignore
                    dbScope.Dispose()
                    return Error msg
                | _ ->
                    dbContext.SubmitUpdates()
                    dbScope.Complete()
                    return r
            with
            | e -> 
                log.Error e
                return Error "Transaction failed"
        }

    let readDB f =
        try
            f defaultContext
        with
        | e -> 
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

    let withCurrentUser (f : int -> 'a -> 'c) =
        let userId = GetContext().UserSession.GetLoggedInUser() |> Async.RunSynchronously
        match userId with
        | None ->
            failwith "Nobody logged in"
        | Some userId ->
            match userId |> Int32.TryParse with
            | (true, v) -> f v
            | _ -> failwith "UserId was not an integer"
    


    [<Remote>]
    let login email password =
        let matchingUserIds =
            query {
                for user in DB.GetDataContext().Public.Users do
                where (user.Email |> Option.map id = Some email && user.Password = password)
                select ( user.Id,
                         user.Confirmemailguid,
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
        match matchingUserIds |> Seq.toList with
        | [userId, confirmEmailGuid, userValues] ->
            GetContext().UserSession.LoginUser (userId |> string) |> Async.RunSynchronously
            async {
                match confirmEmailGuid with
                | None -> return Ok <| LoggedInUser userValues
                | Some _ -> return Ok <| Guest userValues
            }
        | [] ->
            async {
                return Failure "Email or password is wrong."
            }
        | _ ->
            async {
                return Error "Unexpectedly found more than 1 result"
            }
    
    let createGuestAccount email =
        async {
            return Ok ()
        }
    
    [<Remote>]
    let setPassword password =
        let salt =  generateSalt 64
        let hashedPassword = generateHashWithSalt password salt 1000 64
        let setPasswordSaltAndConfirmEmailGuid password salt confirmEmailGuid userId (dbContext : DB.dataContext) =
            async {
                dbContext.Public.Users.Where(fun user -> user.Id = userId)
                |> Seq.iter (fun user ->
                    user.Password <- password
                    user.Salt <- salt
                    user.Confirmemailguid <- confirmEmailGuid)
                if dbContext.GetUpdates().Length = 1
                then return Ok ()
                else return Error "An error occured"
            }
        setPasswordSaltAndConfirmEmailGuid hashedPassword salt None
        |> withCurrentUser
        |> withTransaction

    [<Remote>]
    let setUserEmail (email : option<string>) =
        let setUserEmail email userId (dbContext : DB.dataContext) =
            async {
                if dbContext.Public.Users.Where(fun user -> user.Email |> Option.map id = email).Count() = 0
                then
                    dbContext.Public.Users
                        .Where(fun user -> user.Id = userId)
                    |> Seq.iter (fun user -> user.Email <- email; user.Confirmemailguid <- Some <| Guid.NewGuid().ToString("N")) //TODO new Email might not exist, old email should be saved
                    return Ok ()
                else return Failure "Email already exists"
            }
        setUserEmail email |> withCurrentUser |> withTransaction
    
    //[<Remote>]
    //let loginUserBySessionGuid sessionGuid =
    //    log.Debug(sprintf "(sessionGuid = %s)" sessionGuid)
    //    match Database.tryGetUserIdBySessionGuid sessionGuid |> readDB with
    //    | None ->
    //        log.Debug(sprintf "(sessionGuid = %s) = %b" sessionGuid false)
    //        async { return false }
    //    | Some (UserId userId) ->
    //        GetContext().UserSession.LoginUser (userId |> string) |> Async.RunSynchronously
    //        log.Debug(sprintf "(sessionGuid = %s) = true" sessionGuid)
    //        async { return true }
    
    //[<Remote>]
    //let setSessionGuid oSessionGuid =
    //    let userId = withCurrentUser ()
    //    async {
    //        return
    //            userId
    //            |> Database.setSessionGuid oSessionGuid
    //            |> withTransaction
    //            }
    
    //[<Remote>]
    //let register (email : string) (password : string) =
    //    async {
    //        try
    //            log.Debug (sprintf "(email = %s, password = %s)" email password)
    //            let emailRegexStr = """^(([^<>()\[\]\\.,;:\s@"]+(\.[^<>()\[\]\\.,;:\s@"]+)*)|(".+"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$"""
    //            if not <| Regex.IsMatch(email, emailRegexStr)
    //            then return fail "Email-Adresse scheint unzulässig zu sein."
    //            elif password = ""
    //            then return fail "Passwort darf nicht leer sein."
    //            else
    //                match Database.userEmailExists email |> readDB with
    //                | true ->
    //                    return fail "Diese Email-Adresse ist schon registriert."
    //                | false ->
    //                    let salt = generateSalt 64
    //                    let hashedPassword = generateHashWithSalt password salt 1000 64
    //                    let confirmEmailGuid = Guid.NewGuid().ToString("N")
    //                    Database.insertUser (Some email) hashedPassword salt (Some confirmEmailGuid) None System.DateTime.Now
    //                    |> andThen (fun user dbContext ->
    //                        Database.insertUserValues emptyUserValues (UserId user.Id) dbContext
    //                        Database.insertLastLogin DateTime.Now (UserId user.Id) dbContext)
    //                    |> withTransaction
    //                    sendEmail
    //                        Settings.EmailUsername
    //                        Settings.DomainName
    //                        email
    //                        (t German PleaseConfirmYourEmailAddressEmailSubject)
    //                        (String.Format(t German PleaseConfirmYourEmailAddressEmailBody, email, confirmEmailGuid))
    //                        []
    //                    return ok "Please confirm your email"
    //        with
    //        | e ->
    //            log.Error("", e)
    //            return fail "An error occured."
    //    }

    //[<Remote>]
    //let confirmEmail email confirmEmailGuid =
    //    async {
    //        return
    //            (fun dbContext ->
    //                log.Debug (sprintf "(email = %s, guid = %s)" email confirmEmailGuid)
    //                match Database.tryGetConfirmEmailGuid email |> readDB with
    //                | None -> ok "Email already confirmed"
    //                | Some guid when guid = confirmEmailGuid ->
    //                    Database.setConfirmEmailGuid email None dbContext 
    //                    ok "Email has been confirmed."
    //                | Some _ ->
    //                    fail "Unknown confirmEmailGuid")
    //            |> withTransaction
    //    }

    //[<Remote>]
    //let getDocumentNames () =
    //    let userId = withCurrentUser ()
    //    async {
    //        return
    //            userId
    //            |> Database.getDocumentNames
    //            |> readDB
    //            }

    //[<Remote>]
    //let overwriteDocument (document : Document) =
    //    let userId = withCurrentUser ()
    //    async {
    //        return
    //            userId
    //            |> Database.overwriteDocument document
    //            |> withTransaction
    //            }

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

    
