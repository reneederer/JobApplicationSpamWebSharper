namespace JobApplicationSpam.Server

open JobApplicationSpam.Types
open System.IO
open System.Text.RegularExpressions
open System.IO.Compression


module Path =
    let getExtensionNoDot filePath =
        match Path.GetExtension filePath with
        | "" -> ""
        | s when s.StartsWith "."-> s.Substring(1)
        | s -> s

module FileConverter =
    open System
    open System.Threading
    open PdfSharp.Pdf
    open PdfSharp.Pdf.IO

    let private log = log4net.LogManager.GetLogger(System.Reflection.MethodBase.GetCurrentMethod().GetType())

    let applyRec path f : list<'a> =
        let rec applyRec' currentDir =
            let directories = Directory.EnumerateDirectories(Path.Combine(path, currentDir))
            let xs =
                [ for directory in directories do
                    yield! applyRec' (Path.Combine(currentDir, Path.GetFileName(directory)))
                ]

            let files = Directory.EnumerateFiles(Path.Combine(path, currentDir))
            xs
            @
            [ for file in files do
                yield f currentDir (Path.GetFileName(file)) ]
        applyRec' ""


    let replaceInString (text1 : string) (map : list<string * string>) =
        let rec replaceInString' (text : string) =
            List.fold
                (fun (state:string) (k: string, v: string) ->
                    let replace (s : string) (k1 : string) (v1: string) = 
                        let replacedV =
                            if v1.Contains "$" && s.Contains k1
                            then replaceInString' v1
                            else v1

                        if replacedV = ""
                        then s.Replace(k1 + " ", "").Replace(k1, "")
                        else
                            let replaceValue = System.Security.SecurityElement.Escape(replacedV)
                            s.Replace(k1, replaceValue)
                    replace state k v
                )
                text
                map
        replaceInString' text1



    let replaceInFile path map =
        let content = File.ReadAllText(path)
        let replacedText = replaceInString content map
        File.WriteAllText(path, replacedText)

    

    let rec replaceInExtractedOdtDirectory path map =
        applyRec
            path
            (fun currentDir currentFile ->
                if Path.GetExtension(currentFile).ToLower() = (".xml")
                then replaceInFile (Path.Combine(path, currentDir, currentFile)) map
                else ()
            ) |> ignore


    
    let replaceInOdt odtPath extractedOdtDirectory replacedOdtDirectory map =
        log.Debug (sprintf "(odtPath=%s, extractedOdtDirectory=%s, replacedOdtDirectory=%s)" odtPath extractedOdtDirectory replacedOdtDirectory)
        let odtFileName = Path.GetFileName(odtPath)
        if not <| Directory.Exists(replacedOdtDirectory) then Directory.CreateDirectory(replacedOdtDirectory) |> ignore
        let replacedOdtPath = Path.Combine(replacedOdtDirectory, odtFileName)
        if Directory.Exists extractedOdtDirectory then Directory.Delete(extractedOdtDirectory, true)
        if File.Exists(replacedOdtPath) then File.Delete(replacedOdtPath)
        ZipFile.ExtractToDirectory(odtPath, extractedOdtDirectory)
        replaceInExtractedOdtDirectory extractedOdtDirectory map
        ZipFile.CreateFromDirectory(extractedOdtDirectory, replacedOdtPath)
        Directory.Delete(extractedOdtDirectory, true)
        log.Debug (sprintf "(odtPath=%s, extractedOdtDirectory=%s, replacedOdtDirectory=%s) = %s" odtPath extractedOdtDirectory replacedOdtDirectory replacedOdtPath)
        replacedOdtPath
    

    
    let private convertTo fileType filePath =
        let rec convertToOdt' tries =
            let outputPath = Path.ChangeExtension(filePath, fileType)
            File.Delete(outputPath)
            use process1 = new System.Diagnostics.Process()
            process1.StartInfo.FileName <- Settings.Python
            process1.StartInfo.UseShellExecute <- false
            process1.StartInfo.Arguments <-
                sprintf
                    """ "%s" --format %s --output="%s" "%s" """
                    Settings.Unoconv
                    fileType
                    outputPath
                    filePath
            printfn "%s" process1.StartInfo.Arguments
            process1.StartInfo.CreateNoWindow <- true
            process1.Start() |> ignore
            process1.WaitForExit()
            if File.Exists outputPath
            then outputPath
            else
                if tries > 0
                then
                    Thread.Sleep 5000
                    convertToOdt' (tries - 1)
                else
                    log.Error (sprintf "(filePath = %s) failed to convert" filePath)
                    failwith "Could not convert file to odt: " + filePath
        convertToOdt' 5
    
    let convertToPdf = convertTo "pdf"
    let convertToOdt = convertTo "odt"
    
    let mergePdfs (pdfPaths : list<string>) (outputPath : string) =
        log.Debug (sprintf "(pdfPaths = %A, outputPath = %s)" pdfPaths outputPath)
        Directory.CreateDirectory(Path.GetDirectoryName(outputPath)) |> ignore
        use outputDocument = new PdfDocument ()
        for pdfPath in pdfPaths do
            let inputDocument = PdfReader.Open(pdfPath, PdfDocumentOpenMode.Import)
            for page in inputDocument.Pages do
                outputDocument.AddPage page |> ignore
        outputDocument.Save outputPath
        log.Debug (sprintf "(pdfPaths = %A, outputPath = %s) = ()" pdfPaths outputPath)

