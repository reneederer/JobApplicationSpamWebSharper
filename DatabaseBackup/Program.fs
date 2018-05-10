open System
open System.Threading
open System.IO
open System.IO.Compression
open System.Diagnostics

[<EntryPoint>]
let main argv = 
    let backupDirectory = "c:/users/rene/backup3"
    let tmpDirectory = Path.Combine(backupDirectory, "tmp")
    let txtFile = Path.Combine(tmpDirectory, "backup.txt")
    let zipFile = Path.Combine(backupDirectory, "backup.zip")
    Directory.CreateDirectory (tmpDirectory) |> ignore
    File.Delete txtFile
    File.Delete zipFile
    use proc = new Process()
    proc.StartInfo.FileName <- @"C:\Program Files\PostgreSQL\10\bin\pg_dumpall.exe"
    proc.StartInfo.Arguments <- sprintf "-f %s -U postgres  -h localhost -p 5432" txtFile

    printfn "Starting backup"
    proc.StartInfo.RedirectStandardOutput <- true
    proc.StartInfo.UseShellExecute <- false
    proc.Start() |> ignore
    proc.WaitForExit()
    proc.Dispose()
    if File.Exists txtFile
    then
        ZipFile.CreateFromDirectory(tmpDirectory, zipFile, CompressionLevel.Optimal, false) 
        if File.Exists zipFile
        then
            Directory.Delete(tmpDirectory, true) |> ignore
            printfn "Backup finished."
        else
            Directory.Delete backupDirectory |> ignore
            printfn "Error, no backup was created."
    0
