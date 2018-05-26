namespace JobApplicationSpam
module Dependencies =
    open WebSharper

    [<Require(typeof<JQuery.Resources.JQuery>)>]
    [<Sealed>]
    type TwitterBootstrap() =
        inherit Resources.BaseResource("https://stackpath.bootstrapcdn.com/bootstrap/4.1.1/",
            "js/bootstrap.min.js", "css/bootstrap.min.css")

    [<assembly: Require(typeof<TwitterBootstrap>)>]
    do ()