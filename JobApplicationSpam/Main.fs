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
    open WebSharper.UI.Next.Html

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
    open Types
    open System.Reflection
    open WebSharper.Sitelets
    open System.Transactions
    open Path

    let log = LogManager.GetLogger(MethodBase.GetCurrentMethod().GetType())

    let HomePage ctx =
        Templating.Main ctx EndPoint.Home "Bewerbungsspam" [
            div [client <@ Client.Main() @>]
        ]

    [<Website>]
    let Main =
        Server.init()
        Application.MultiPage (fun (ctx : Context<EndPoint>) endpoint ->
            match endpoint with
            | EndPoint.Home ->
                HomePage ctx
            | EndPoint.Upload fileUpload ->
                (*
                1. Dateityp prüfen
                2. Wenn Pdf dann Datei speichern und Datenbankeinträge schreiben
                   Wenn Odt dann Datei speichern und Datenbankeinträge schreiben
                   Wenn konvertierbar zu Odt dann konvertieren zu Odt, Datei speichern und Datenbankeinträge schreiben
                   Wenn konvertierbar zu Pdf dann konvertieren zu Pdf, Datei speichern und Datenbankeinträge schreiben
                Datei speichern und Datenbankeinträge speichern:
                *)

                let findFreeFileName (file : string) (documentId : int) =
                    let fileNameWithoutExtension = Path.GetFileNameWithoutExtension(file)
                    let extension = Path.getExtensionNoDot file
                    let existingFileNames =
                        let filePageNames = Server.getFilePageNames documentId |> Async.RunSynchronously
                        log.Debug (sprintf "filePageNames: %A" filePageNames)

                        filePageNames
                        |> List.filter (fun x -> Path.getExtensionNoDot(x) = extension)
                        |> List.filter (fun x -> Path.GetFileNameWithoutExtension(x) = fileNameWithoutExtension)
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
                    let tmpFilePath = Path.Combine(Settings.TmpDir, Guid.NewGuid().ToString("N"), file.FileName)
                    Directory.CreateDirectory (Path.GetDirectoryName tmpFilePath) |> ignore
                    file.SaveAs tmpFilePath
                    match Path.getExtensionNoDot file.FileName with
                    | "doc"
                    | "docx" -> FileConverter.convertToOdt tmpFilePath
                    | "jpg"
                    | "gif"
                    | "png" -> FileConverter.convertToPdf tmpFilePath
                    | "pdf" -> tmpFilePath
                    | "odt" -> tmpFilePath
                    | s -> failwith "unsupported format" + s

                let getSaveToUserDir userId =
                    Path.Combine(Settings.UserDir, string userId)

                for file in ctx.Request.Files do
                    try
                        log.Debug("file.Filename: " + file.FileName)
                        let tmpFilePath = convertAndSaveTemporarily file
                        log.Debug("tmpFilePath: " + tmpFilePath)
                        let saveFilePath = getFilePath_testIfIdenticalFileExists tmpFilePath [] (Path.Combine(Settings.UserDir, string fileUpload.userId))
                        log.Debug("saveFilePath: " + saveFilePath)
                        let saveFileName = findFreeFileName file.FileName fileUpload.documentId
                        log.Debug("saveFileName: " + saveFileName)
                        //use transactionScope = new TransactionScope()
                        match Server.addFilePage saveFileName saveFilePath fileUpload.documentId |> Async.RunSynchronously with
                        | Ok _ ->
                            let saveToUserDir =  getSaveToUserDir fileUpload.userId
                            if not <| Directory.Exists saveToUserDir
                            then Directory.CreateDirectory saveToUserDir |> ignore
                            File.Move(tmpFilePath, Path.Combine(saveToUserDir, saveFilePath))
                            //transactionScope.Complete()
                        | _ ->
                            ()
                            //transactionScope.Dispose()
                     with
                     | e -> log.Error ("", e)
                Content.RedirectPermanentToUrl "/"
        )



















