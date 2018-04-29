namespace JobApplicationSpam

open WebSharper
open WebSharper.Sitelets
open WebSharper.UI.Next
open WebSharper.UI.Next.Server


type EndPoint =
    | [<EndPoint "/">] Home

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

    log4net.Config.XmlConfigurator.Configure(new FileInfo("log4net.config")) |> ignore

    let HomePage ctx =
        Templating.Main ctx EndPoint.Home "Bewerbungsspam" [
            div [client <@ Client.Main() @>]
        ]

    [<Website>]
    let Main =
        Application.MultiPage (fun ctx endpoint ->
            match endpoint with
            | EndPoint.Home -> HomePage ctx
        )
