namespace JobApplicationSpam

open WebSharper
open WebSharper.Sitelets
open WebSharper.UI.Next
open WebSharper.UI.Next.Server
open Types
open System.Web
open WebSharper.Sitelets.Http


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
                for file in ctx.Request.Files do
                    file.SaveAs ("c:/users/rene/" + file.FileName + (string fileUpload.userId))
                Content.RedirectPermanentToUrl "/"
        )
