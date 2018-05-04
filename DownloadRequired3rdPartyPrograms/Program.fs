open System.Net
open System.IO
open System
open System.Text
open System.Configuration

[<EntryPoint>]
let main argv = 
    let download (uriStr : string) =
        let uri = Uri uriStr
        let filePath = Path.Combine("c:/users/Administrator/Downloads/", Path.GetFileName(uri.LocalPath))
        try
            if not (File.Exists filePath)
            then
                use  webClient = new WebClient()
                printfn "\nStarted %s" uriStr
                webClient.DownloadFile(uri, filePath)
                printfn "Finished %s\n" uriStr
        with
        | e -> printfn "Failed to download %s.\r\n%s" uri.OriginalString (e.ToString())

    printfn "Downloads starten? (y/n)"
    match Console.ReadKey().KeyChar with
    | 'y' | 'Y' | 'j' | 'J' -> 
        [ "https://ftp.fau.de/tdf/libreoffice/stable/6.0.3/win/x86_64/LibreOffice_6.0.3_Win_x64.msi"
          "https://get.enterprisedb.com/postgresql/postgresql-10.3-3-windows-x64.exe"
          "https://autohotkey.com/download/1.1/AutoHotkey_1.1.28.02_setup.exe"
          "https://www.voidtools.com/Everything-1.4.1.895.x64-Setup.exe"
          "http://download.microsoft.com/download/3/3/2/332D9665-37D5-467A-84E1-D07101375B8C/NDP472-KB4054531-Web.exe"
          "https://github-production-release-asset-2e65be.s3.amazonaws.com/46080325/4f614688-32d2-11e8-81d2-05aad1bac404?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Credential=AKIAIWNJYAX4CSVEH53A%2F20180502%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20180502T192014Z&X-Amz-Expires=300&X-Amz-Signature=f023fd3101a30c1d4fbffee6ca2f1ea8519b36d4378375ef42a05ef0bc0293b0&X-Amz-SignedHeaders=host&actor_id=0&response-content-disposition=attachment%3B%20filename%3Dwin-acme.v1.9.10.1.zip&response-content-type=application%2Foctet-stream"
          "https://webpihandler.azurewebsites.net/web/handlers/webpi.ashx/getinstaller/urlrewrite2.appids"
          "http://downloads.zoolz.com/zoolz2/ZoolzSetup.exe"
        ]
        |> List.iter download
    | _ -> ()
    (*
    let request = WebRequest.Create("ftp://www.contoso.com/test.htm") :?> FtpWebRequest
    request.Method <- WebRequestMethods.Ftp.UploadFile
    request.Credentials <- new NetworkCredential("reneederer", "Steinmetzstr9!@#$")
    let sourceStream = new StreamReader("testfile.txt")
    sourceStream.Close() 
    let fileContents = Encoding.UTF8.GetBytes(sourceStream.ReadToEnd()) 
    request.ContentLength <- int64 fileContents.Length 

    use requestStream = request.GetRequestStream() 
    requestStream.Write(fileContents, 0, fileContents.Length) 
    requestStream.Close() 

    use response = request.GetResponse() :?> FtpWebResponse

    Console.WriteLine("Upload File Complete, status {0}", response.StatusDescription) 

    response.Close() 
    *)
    0
