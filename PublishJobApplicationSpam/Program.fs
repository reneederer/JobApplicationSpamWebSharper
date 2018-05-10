open System.Net
open System.IO
open System
open System.Text
open System.Configuration
open FSharp.Configuration
open FSharp.Configuration

type Settings = AppSettings<"App.config">

[<EntryPoint>]
let main argv = 
    let publish rootDirectory (username : string) (password : string) (rootUri : string) =
        ServicePointManager.ServerCertificateValidationCallback <- fun _ _ _ _ -> true
        let rec publish' relativeDirectoryPath =
            let fullDirectoryPath = Path.Combine(rootDirectory, relativeDirectoryPath)

            if relativeDirectoryPath <> ""
            then
                let uri = new Uri(Path.Combine(rootUri, relativeDirectoryPath))
                let request = WebRequest.Create(new Uri(Path.Combine(rootUri, relativeDirectoryPath))) :?> FtpWebRequest
                request.EnableSsl <- true
                request.Method <- WebRequestMethods.Ftp.MakeDirectory
                request.Credentials <- new NetworkCredential(username, password)
                request.GetResponse() |> ignore


            let files = Directory.EnumerateFiles(fullDirectoryPath) |> Seq.map Path.GetFileName
            for currentFile in files do
                let request = WebRequest.Create(new Uri(Path.Combine(rootUri, relativeDirectoryPath, currentFile))) :?> FtpWebRequest
                request.EnableSsl <- true
                request.Method <- WebRequestMethods.Ftp.UploadFile  
                request.Credentials <- new NetworkCredential(username, password)
                use sourceStream = new StreamReader(Path.Combine(fullDirectoryPath, currentFile))  
                let fileContents = Encoding.UTF8.GetBytes(sourceStream.ReadToEnd())  
                sourceStream.Close()  
                request.ContentLength <- int64 fileContents.Length

                let requestStream = request.GetRequestStream()  
                requestStream.Write(fileContents, 0, fileContents.Length)  
                requestStream.Close()  

                use response = request.GetResponse() :?> FtpWebResponse
                response.Close()
            let directories = Directory.EnumerateDirectories(fullDirectoryPath)
            for currentDirectory in directories do
                publish' (currentDirectory.Substring(rootDirectory.Length + 1))
            ()
        publish' ""
    try
        publish
            "C:/Users/rene/Documents/Visual Studio 2017/Projects/JobApplicationSpam/Publish/meintest"
            Settings.ServerUsername
            Settings.ServerPassword
            Settings.FtpServer.OriginalString
    with
    | e ->
        printfn "%A" e
    0