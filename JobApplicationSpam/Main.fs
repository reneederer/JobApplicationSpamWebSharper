namespace JobApplicationSpam

open WebSharper
open WebSharper.Sitelets
open WebSharper.UI.Next
open WebSharper.UI.Next.Server
open Types
open System.Web
open WebSharper.Sitelets.Http
open System


type EndPoint =
    | [<EndPoint "/">] Home
    | [<EndPoint "POST /Upload">] Upload of fileUpload : FileUpload

module Templating =
    type MainTemplate = Templating.Template<"templates/Main.html">

    let Main ctx action (title: string) (body: Doc list) =
        Content.Page(
            MainTemplate()
                .Title(title)
                .Body(body)
                .Doc()
        )

module Site =
    open WebSharper.UI.Next.Html
    open System.IO
    open log4net
    open System.Reflection
    open System.Transactions

    let log = LogManager.GetLogger(MethodBase.GetCurrentMethod().GetType())

    let HomePage ctx =
        Templating.Main ctx EndPoint.Home "Bewerbungsspam" [
            div [client <@ Client.Main() @>]
        ]

    [<Website>]
    let Main =
        Application.MultiPage (fun (ctx : Context<EndPoint>) endpoint ->
            match endpoint with
            | EndPoint.Home ->
                HomePage ctx
            | EndPoint.Upload fileUpload ->
                let findFreeFileName (file : string) (documentId : int) =
                    let fileNameWithoutExtension = Path.GetFileNameWithoutExtension(file)
                    let extension = Path.getExtensionNoDot file
                    let existingFileNames =
                        let filePageNames = Server.getFilePageNames documentId |> Async.RunSynchronously
                        log.Debug (sprintf "filePageNames: %A" filePageNames)

                        filePageNames
                        |> List.filter (fun x -> Path.getExtensionNoDot(x) = extension)
                    let rec findFreeFileName' i =
                        let name = sprintf "%s%s%s" fileNameWithoutExtension (if i = 0 then "." else sprintf " (%i)." i) extension
                        if List.contains name existingFileNames
                        then findFreeFileName' (i + 1)
                        else name
                    findFreeFileName' 0
                
                let findFreeFilePath (file : string) (baseDir : string) =
                    let fileNameWithoutExtension = Path.GetFileNameWithoutExtension(file)
                    let extension = Path.getExtensionNoDot file
                    let fileNameWithIndex fileNameWithoutExtension index extension =
                        if index = 0
                        then fileNameWithoutExtension + "." + extension
                        else
                            sprintf
                                "%s (%i).%s"
                                fileNameWithoutExtension
                                index
                                extension
                    Seq.initInfinite id
                    |> Seq.find (fun i -> not <| File.Exists(Path.Combine(baseDir, fileNameWithIndex fileNameWithoutExtension i extension)))
                    |> fun i -> fileNameWithIndex fileNameWithoutExtension i extension

                let tryFindIdenticalFile (filePath : string) (filePaths : list<string>) =
                    //Some filePaths.[0]
                    None

                let getFilePath_testIfIdenticalFileExists filePath potentiallyIdenticalFiles baseDir =
                    match tryFindIdenticalFile filePath potentiallyIdenticalFiles with
                    | None -> findFreeFilePath filePath baseDir
                    | Some path -> path

                let convertAndSaveTemporarily (file : IPostedFile) =
                    let tmpFilePath = Path.Combine(Settings.DataDir, "tmp", Guid.NewGuid().ToString("N"), file.FileName)
                    Directory.CreateDirectory (Path.GetDirectoryName tmpFilePath) |> ignore
                    file.SaveAs tmpFilePath
                    match Path.getExtensionNoDot file.FileName with
                    | "doc"
                    | "docx" -> FileConverter.convertToOdt tmpFilePath
                    | "jpg"
                    | "gif"
                    | "png" -> FileConverter.convertToPdf tmpFilePath
                    | "pdf" -> tmpFilePath
                    | "odt" ->
                        Server.replaceVariables
                            tmpFilePath
                            { id = 0
                              gender = Gender.Female
                              degree = "Dr. "
                              name = "René Ederer"
                              street = "Raabstr. 24A"
                              postcode = "90429"
                              city = "Nürnberg"
                              email = "rene.ederer.nbg@gmail.com"
                              phone = "phonee"
                              mobilePhone = "mobill"
                            }
                            { company = "BFI"
                              gender = Gender.Male
                              degree = ""
                              firstName = "Michi"
                              lastName = "Meier"
                              street = "Hanbuchener Str. 99"
                              postcode = "90491"
                              city = "Nürnberg"
                              email = "michi@bfi.com"
                              phone = "0911 92381123"
                              mobilePhone = "0151 98182391"
                            }
                            { id = 2
                              pages = []
                              jobName = "Fachinfo"
                              name = "SomeDoc"
                              customVariables = ""
                              emailSubject = "Titel der Email"
                              emailBody = "Sehr geehrter Herr $chefNachname"
                            }
                        |> Async.RunSynchronously
                    | s -> failwith "unsupported format" + s

                let getSaveToUserDir userId =
                    Path.Combine(Settings.DataDir, "user", string userId)
                
                try
                    let file = ctx.Request.Files |> Seq.item 0
                    match Path.getExtensionNoDot(file.FileName) with
                    | "txt" ->
                        Content.Json (Failure "Unrecognized file type")
                    | _ ->
                        let tmpFilePath = convertAndSaveTemporarily file
                        let saveFilePath = getFilePath_testIfIdenticalFileExists tmpFilePath [] (Path.Combine(Settings.DataDir, "user", string fileUpload.userId))
                        let saveFileName = findFreeFileName tmpFilePath fileUpload.documentId
                        //use transactionScope = new TransactionScope()
                        let fullSaveFilePath = Path.Combine("user", string fileUpload.userId, saveFilePath)
                        match Server.addFilePage saveFileName fullSaveFilePath fileUpload.documentId |> Async.RunSynchronously with
                        | Ok _ ->
                            let saveToUserDir =  getSaveToUserDir fileUpload.userId
                            if not <| Directory.Exists saveToUserDir
                            then Directory.CreateDirectory saveToUserDir |> ignore
                            File.Move(tmpFilePath, Path.Combine(saveToUserDir, saveFilePath))
                            Content.Json (Ok (fullSaveFilePath, saveFileName, 0))
                            //transactionScope.Complete()
                        | _ ->
                            Content.Json (Failure "Adding file page failed.")
                            //transactionScope.Dispose()
                 with
                 | e ->
                    log.Error ("", e)
                    Content.Json Error
        )



















