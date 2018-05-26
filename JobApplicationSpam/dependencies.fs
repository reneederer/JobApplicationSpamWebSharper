namespace JobApplicationSpam
module Dependencies =
    open WebSharper

    [<Require(typeof<JQuery.Resources.JQuery>)>]
    [<Sealed>]
    type TwitterBootstrap() =
        inherit Resources.BaseResource("https://stackpath.bootstrapcdn.com/bootstrap/4.1.1/",
            "js/bootstrap.min.js", "css/bootstrap.min.css")

    [<Require(typeof<JQuery.Resources.JQuery>)>]
    [<Sealed>]
    type FontAwesome() =
        inherit Resources.BaseResource("https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css")

    [<assembly: Require(typeof<TwitterBootstrap>)>]
    [<assembly: Require(typeof<FontAwesome>)>]
    do ()