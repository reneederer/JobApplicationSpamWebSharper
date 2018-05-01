namespace JobApplicationSpam

open Types
open System.IO


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

    let odtToPdf (odtPath : string) =
        let outputPath = Path.ChangeExtension(odtPath, ".pdf")
        File.Delete(outputPath)
        use proc = new System.Diagnostics.Process()
        proc.StartInfo.FileName <- Settings.Python
        proc.StartInfo.UseShellExecute <- false
        proc.StartInfo.Arguments <-
            sprintf
                """ "%s" --format pdf -P PaperFormat=A4 -eUseLossLessCompression=true "%s" """
                Settings.Unoconv
                odtPath
        proc.StartInfo.CreateNoWindow <- true
        proc.Start() |> ignore
        proc.WaitForExit()
        let outputPath = Path.ChangeExtension(odtPath, ".pdf")
        if File.Exists outputPath
        then
            Some outputPath
        else
            log.Error (sprintf "(odtPath = %s) failed to convert" odtPath)
            None
    
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
            then
                outputPath
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

