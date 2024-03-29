﻿namespace JobApplicationSpam

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI.Next
open WebSharper.UI.Next.Client
open WebSharper.UI.Next.Html
open WebSharper.JQuery
open JobApplicationSpam.Server
open Types

type State =
    { documents : list<Document>
      sentApplications : list<SentApplication>
      activeFileName : string
      activeDocumentName : string
      employer : Employer
      user : User
      login : Login
      register : Register
      changePassword : ChangePassword
      forgotPassword : ForgotPassword
      changeEmail : ChangeEmail
      sentApplicationsModalValues : SentApplication
    }

[<JavaScript>]
module Url =
    let getParameters (url : string) =
        let parameterString = url.Substring(url.IndexOf("?") + 1)
        let parameters = parameterString.Split([|'&'|])
        [ for parameter in parameters do
            match parameter.Split([|'='|]) with
            | [| x1; x2 |] -> yield (x1, x2)
            | _ -> yield ("", "")
        ] |> Map.ofList


[<JavaScript>]
module List =
    let rec removeFirst (f : 'a -> bool) xs =
        match xs with
        | [] -> []
        | x::rest ->
            if f x
            then rest
            else x :: removeFirst f rest
    let rec moveDown (f : 'a -> bool) xs =
        match xs with
        | [] -> []
        | [x] -> [x]
        | x1::x2::rest ->
            if f x1
            then x2::x1::rest
            else x1::moveDown f (x2::rest)
    let rec moveUp (f : 'a -> bool) xs =
        match xs with
        | [] -> []
        | [x] -> [x]
        | x1::x2::rest ->
            if f x2
            then x2::x1::rest
            else x1::moveUp f (x2::rest)
    let rec replace i v xs =
        match i, xs with
        | _, [] -> []
        | 0, x::rest ->
            v::rest
        | _, x::rest ->
            x::(replace (i - 1) v rest)
    let rec mapAtOrDefault i (f : 'a -> 'b) (d : 'b) xs =
        match i, xs with
        | 0, x::_ ->
            f x
        | _, _::rest -> mapAtOrDefault (i - 1) f d rest
        | _ -> d

    let rec mapInner (p : 'a -> bool) (f : 'a -> 'a) (xs : list<'a>) : list<'a> =
        match xs with
        | [] -> []
        | (x :: rest) ->
            (if p x
            then f x
            else x)
            :: (mapInner p f rest)
    let mapInnerDocument (s : State) (f : Document -> Document) (xs : list<Document>) : State =
        let rec mapInnerDocument' (s : State) (f : Document -> Document) (xs : list<Document>) : list<Document> =
            match xs with
            | [] -> []
            | (x :: rest) ->
                (if x.name = s.activeDocumentName
                then f x
                else x)
                :: (mapInnerDocument' s f rest)
        { s with documents = mapInnerDocument' s f xs }

[<JavaScript>]
module Client =
    open System.Net.Http
    open System
    open System.Collections.Generic

    type Templates = Templating.Template<"templates/Templates.html">
    type MainTemplate = Templating.Template<"templates/Main.html">

    type UserRefs =
        { gender : IRef<Gender>
          degree : IRef<string>
          firstName : IRef<string>
          lastName : IRef<string>
          street : IRef<string>
          postcode : IRef<string>
          city : IRef<string>
          email : IRef<string>
          phone : IRef<string>
          mobilePhone : IRef<string>
        }

    type EmployerRefs =
        { company : IRef<string>
          gender : IRef<Gender>
          degree : IRef<string>
          firstName : IRef<string>
          lastName : IRef<string>
          street : IRef<string>
          postcode : IRef<string>
          city : IRef<string>
          email : IRef<string>
          phone : IRef<string>
          mobilePhone : IRef<string>
        }
    type EmailRefs =
        { subject : IRef<string>
          body : IRef<string>
        }
    type LoginRefs =
        { email : IRef<string>
          password : IRef<string>
        }
    type RegisterRefs =
        { email : IRef<string>
          password : IRef<string>
        }
    type ChangePasswordRefs =
        { password : IRef<string>
        }
    type ForgotPasswordRefs =
        { email : IRef<string>
        }
    type ChangeEmailRefs =
        { email : IRef<string>
        }
    type StateRefs =
        { documents : IRef<list<Document>>
          jobName : IRef<string>
          user : UserRefs
          employer : EmployerRefs
          email : EmailRefs
          activeFileName : IRef<string>
          activeDocumentName : IRef<string>
          pages : IRef<list<Page>>
          sentApplications : IRef<list<SentApplication>>
          login : LoginRefs
          register : RegisterRefs
          changePassword : ChangePasswordRefs
          forgotPassword : ForgotPasswordRefs
          changeEmail : ChangeEmailRefs
          activeSidebarItems : Var<list<string>>
          mainbarLoginDiv : Var<Doc>
          customVariables : IRef<list<CustomVariable>>
          customVariablesSaved : IRef<list<CustomVariable>>
        }
    let state : Var<State> =
        Var.Create
            { documents =
                [
                  { pages =
                        [ FilePage {name = "Bewerbungsunterlagen.pdf"; size = 0; path = ""}
                          FilePage {name = "Meine_ganz_tollen_Bewerbungsunterlagen.pdf"; size = 0; path = ""}];
                    name="Doc 1"
                    customVariables = []
                    jobName = "Fachinformatiker"
                    id = 1
                    emailSubject = ""
                    emailBody = ""
                  }
                  { pages = [ FilePage {name = "XX"; size = 0; path = ""};  FilePage {name = "YY"; size = 0; path = ""};  FilePage {name = "ZZ"; size = 0; path = ""}];
                    name="Doc 2"
                    customVariables = []
                    jobName = "Fachinformatiker"
                    id = 2
                    emailSubject = ""
                    emailBody = ""
                  }
                  { pages = [ FilePage {name = "D"; size = 0; path = ""};  FilePage {name = "E"; size = 0; path = ""};  FilePage {name = "F"; size = 0; path = ""}];
                    name="Doc 3"
                    customVariables = []
                    jobName = "Fachinformatiker"
                    id = 3
                    emailSubject = ""
                    emailBody = ""
                  }
                  { pages = [ FilePage {name = "u"; size = 0; path = ""};  FilePage {name = "v"; size = 0; path = ""}];
                    name="Doc 4"
                    customVariables = []
                    jobName = "Fachinformatiker"
                    id = 4
                    emailSubject = ""
                    emailBody = ""
                  }
                ]
              sentApplications = []
              activeFileName = ""
              activeDocumentName = "Doc 1"
              employer = emptyEmployer
              user = Guest emptyUserValues
              login = { email = "rene.ederer.nbg@gmail.com"; password = "1234" }
              register = { email = ""; password = "" }
              changePassword = { password = "" }
              forgotPassword = { email = ""; }
              changeEmail = { email = ""; }
              sentApplicationsModalValues =
                  { employer = emptyEmployer
                    userValues = emptyUserValues
                    jobName = ""
                    statusHistory = []
                    emailSubject = ""
                    emailBody = ""
                    customVariables = []
                  }
            }
    
    let stateRefs =
        { documents =
            state.Lens
                (fun s -> s.documents)
                (fun s v ->
                    if v <> []
                    then
                        JS.Alert(v.Head.name)
                        { s with
                            documents = v
                            activeDocumentName = v.Head.name
                        }
                    else s
                )
          sentApplications =
            state.Lens
                (fun s -> s.sentApplications)
                (fun s v ->
                    { s with sentApplications = v }
                )
          pages =
              state.Lens
                  (fun s ->
                      let documentIndex = s.documents |> List.findIndex (fun x -> x.name = s.activeDocumentName)
                      let xs = List.mapAtOrDefault documentIndex (fun (x : Document) -> x.pages) [] s.documents
                      xs)
                  (fun s (v : list<Page>) ->
                      let documentIndex = s.documents |> List.findIndex (fun x -> x.name = s.activeDocumentName)
                      if List.length s.documents <= documentIndex
                      then s
                      else { s with documents = List.replace documentIndex {s.documents.[documentIndex] with pages = v} s.documents })
          jobName =
              state.Lens
                  (fun s ->
                      let documentIndex = s.documents |> List.findIndex (fun x -> x.name = s.activeDocumentName)
                      s.documents.[documentIndex].jobName)
                  (fun s v ->
                      let documentIndex = s.documents |> List.findIndex (fun x -> x.name = s.activeDocumentName)
                      { s with documents = List.replace documentIndex { s.documents.[documentIndex] with jobName = v } s.documents })
                      

          user =
            { gender = state.Lens (fun s -> s.user.Values().gender) (fun s v -> { s with user = s.user.Values({ s.user.Values() with gender = v })})
              degree = state.Lens (fun s -> s.user.Values().degree) (fun s v -> { s with user = s.user.Values({ s.user.Values() with degree = v })})
              firstName = state.Lens (fun s -> s.user.Values().firstName) (fun s v -> { s with user = s.user.Values({ s.user.Values() with firstName = v })})
              lastName = state.Lens (fun s -> s.user.Values().lastName) (fun s v -> { s with user = s.user.Values({ s.user.Values() with lastName = v })})
              street = state.Lens (fun s -> s.user.Values().street) (fun s v -> { s with user = s.user.Values({ s.user.Values() with street = v })})
              postcode = state.Lens (fun s -> s.user.Values().postcode) (fun s v -> { s with user = s.user.Values({ s.user.Values() with postcode = v })})
              city = state.Lens (fun s -> s.user.Values().city) (fun s v -> { s with user = s.user.Values({ s.user.Values() with city = v })})
              email = state.Lens (fun s -> s.user.Values().email) (fun s v -> { s with user = s.user.Values({ s.user.Values() with email = v })})
              phone = state.Lens (fun s -> s.user.Values().phone) (fun s v -> { s with user = s.user.Values({ s.user.Values() with phone = v })})
              mobilePhone = state.Lens (fun s -> s.user.Values().mobilePhone) (fun s v -> { s with user = s.user.Values({ s.user.Values() with mobilePhone = v })})
            }
          employer =
            { company = state.Lens (fun s -> s.employer.company) (fun s v -> { s with employer = { s.employer with company = v } })
              gender = state.Lens (fun s -> s.employer.gender) (fun s v -> { s with employer = { s.employer with gender = v } })
              degree = state.Lens (fun s -> s.employer.degree) (fun s v -> { s with employer = { s.employer with degree = v }})
              firstName = state.Lens (fun s -> s.employer.firstName) (fun s v -> { s with employer = { s.employer with firstName = v }})
              lastName = state.Lens (fun s -> s.employer.lastName) (fun s v -> { s with employer = { s.employer with lastName = v }})
              street = state.Lens (fun s -> s.employer.street) (fun s v -> { s with employer = { s.employer with street = v }})
              postcode = state.Lens (fun s -> s.employer.postcode) (fun s v -> { s with employer = { s.employer with postcode = v }})
              city = state.Lens (fun s -> s.employer.city) (fun s v -> { s with employer = { s.employer with city = v }})
              email = state.Lens (fun s -> s.employer.email) (fun s v -> { s with employer = { s.employer with email = v }})
              phone = state.Lens (fun s -> s.employer.phone) (fun s v -> { s with employer = { s.employer with phone = v }})
              mobilePhone = state.Lens (fun s -> s.employer.mobilePhone) (fun s v -> { s with employer = { s.employer with mobilePhone = v }})
            }
          email =
            { subject =
                state.Lens
                    (fun s -> (s.documents |> List.find (fun (x : Document) -> x.name = s.activeDocumentName)).emailSubject)
                    (fun s v -> List.mapInnerDocument s (fun (x : Document) -> { x with emailSubject = v }) s.documents)
              body =
                state.Lens
                    (fun s -> (s.documents |> List.find (fun x -> x.name = s.activeDocumentName)).emailBody)
                    (fun s v -> s.documents |> List.mapInnerDocument s (fun x -> { x with emailBody = v }))
            }
          login =
            { email = state.Lens (fun s -> s.login.email) (fun s v -> { s with login = { s.login with email = v }})
              password = state.Lens (fun s -> s.login.password) (fun s v -> { s with login = { s.login with password = v }})
            }
          register =
            { email = state.Lens (fun s -> s.register.email) (fun s v -> { s with register = { s.register with email = v }})
              password = state.Lens (fun s -> s.register.password) (fun s v -> { s with register = { s.register with password = v }})
            }
          changePassword =
            { password = state.Lens (fun s -> s.changePassword.password) (fun s v -> { s with changePassword = { s.changePassword with password = v }})
            }
          forgotPassword =
            { email = state.Lens (fun s -> s.forgotPassword.email) (fun s v -> { s with forgotPassword = { s.forgotPassword with email = v }})
            }
          changeEmail =
            { email = state.Lens (fun s -> s.changeEmail.email) (fun s v -> { s with changeEmail = { s.changeEmail with email = v }})
            }
          activeFileName = state.Lens (fun s -> s.activeFileName) (fun s v -> { s with activeFileName = v })
          activeDocumentName = state.Lens (fun s -> s.activeDocumentName) (fun s v -> { s with activeDocumentName = v })
          activeSidebarItems = Var.Create [ "DocumentsAndFiles" ]
          mainbarLoginDiv = Var.CreateWaiting ()
          customVariables = Var.Create []
          customVariablesSaved =
              state.Lens
                  (fun s ->
                      let documentIndex = s.documents |> List.findIndex (fun x -> x.name = s.activeDocumentName)
                      let xs = List.mapAtOrDefault documentIndex (fun (x : Document) -> x.customVariables) [] s.documents
                      xs)
                  (fun s (v : list<CustomVariable>) ->
                      let documentIndex = s.documents |> List.findIndex (fun x -> x.name = s.activeDocumentName)
                      if List.length s.documents <= documentIndex
                      then s
                      else
                        { s with documents = List.replace documentIndex {s.documents.[documentIndex] with customVariables = v} s.documents })
        }

    let getMainbarLoginDiv user =
        match user with
        | LoggedInUser values ->
            Templates.MainbarLoginDivLoggedInUser()
                .LoggedInUserInitial(values.email.Substring(0, 1).ToUpper())
                .Click(fun el ev ->
                    stateRefs.activeSidebarItems.Value <- [ "Logout"; "ChangeEmail"; "ChangePassword"; "ForgotPassword"; ]
                )
                .Doc()
        | Guest values when values.email = "" ->
            Templates.MainbarLoginDivGuest()
                .GuestType("Guest")
                .CallToActionText("Register now to avoid data loss")
                .CallToActionClick(fun () ->
                    stateRefs.activeSidebarItems.Value <- ["Login"; "Register" ]
                )
              .Doc()
        | Guest values ->
            Templates.MainbarLoginDivGuest()
                .GuestType("Temporary user")
                .CallToActionText("Confirm your email to avoid data loss")
                .CallToActionClick(fun () ->
                    stateRefs.activeSidebarItems.Value <- [ "Logout"; "ResendConfirmEmailEmail" ]
                )
                .Doc()
    stateRefs.mainbarLoginDiv.Value <- getMainbarLoginDiv (Guest emptyUserValues)
    let currentDocument() =
        let documentIndex = state.Value.documents |> List.findIndex (fun x -> x.name = state.Value.activeDocumentName)
        state.Value.documents.[documentIndex]
    
    let createRadioButton (header : string) (items : list<string * 'a>) (ref : IRef<'a>) =
        div
          ( (labelAttr [attr.style "font-weight: bold" ] [text header] :> Doc)
            ::
            ( items
              |> List.mapi
                    (fun i (label, refValue) ->
                         let id = System.Guid.NewGuid().ToString("N")
                         div
                           [ Doc.Radio [attr.id id] refValue ref
                             labelAttr [ attr.``for`` id ] [ text label ]
                             (if i <> items.Length - 1
                             then br []
                             else divAttr [attr.``class`` "bottom-distanced"] [])
                           ]
                         :> Doc
                    )
            )
          )
    
    let createSelect elementId items ref =
        Doc.SelectDyn
            [attr.id elementId]
            id
            items
            ref
    
    let setUser user =
        state.Value <- { state.Value with user = user }
        stateRefs.mainbarLoginDiv.Value <- getMainbarLoginDiv state.Value.user
        stateRefs.activeSidebarItems.Value <- ["DocumentsAndFiles"]
        stateRefs.mainbarLoginDiv.Value <- getMainbarLoginDiv user

    let main () =
        let horizontalMenuEl = Var.CreateWaiting()
        let horizontalMenuElOffsetTop = Var.Create 0
        JQuery(JS.Document).Ready(
            fun () ->
                horizontalMenuEl.Value <- JS.Document.GetElementById "horizontalMenu"
                horizontalMenuElOffsetTop.Value <- horizontalMenuEl.Value?offsetTop
        ) |> ignore
        JS.Window.Onscroll <-
            fun evt ->
                if JS.Window?pageYOffset >= horizontalMenuElOffsetTop.Value
                    then
                        horizontalMenuEl.Value?classList?add("sticky")
                    else
                        horizontalMenuEl.Value?classList?remove("sticky")

        let ajax (method: string) (url: string) (formData : obj) : Async<string> =
            Async.FromContinuations
                (fun (ok, ko, _) ->
                    JQuery.Ajax(
                        JQuery.AjaxSettings(
                            Url = url,
                            Type = As<JQuery.RequestType> method,
                            ContentType = Union<bool, string>.Union1Of2(false),
                            ProcessData = false,
                            Data = formData,
                            Cache = false,
                            Success = (fun result _ q ->
                                let (r : Result<string * string * int>) = Json.Deserialize(q.ResponseText)
                                match r with
                                | Ok (filePath, fileName, size) ->
                                    JS.Alert("success!"); ok (result :?> string)
                                    stateRefs.pages.Value <- stateRefs.pages.Value @ [FilePage {path = filePath; name = fileName; size = size }]
                                    JS.Document.GetElementById("file")?value <- null
                                | Failure s ->
                                    JS.Alert("failure!" + s); ok (result :?> string)
                                | Error ->
                                    JS.Alert("error!"); ok (result :?> string)
                                ),
                            Error = (fun result s q -> JS.Alert("failure: " + (result.ResponseText))))
                    )
                    |> ignore
                )

        let loadSentApplications () =
            async {
                let! rSentApplications = Server.getSentApplications()
                match rSentApplications with
                | Ok sentApplications ->
                    state.Value <- { state.Value with sentApplications = sentApplications }
                | Failure msg -> JS.Alert(msg)
                | Error -> JS.Alert("Sorry, an error occurred")
            }
        let loadDocuments () =
            async {
                JS.Alert("loading documents!")
                let! rDocuments = Server.getDocuments()
                match rDocuments with
                | Ok documents ->
                    state.Value <- { state.Value with documents = documents; activeDocumentName = documents.Head.name }
                    let mutable shouldWait = 0;
                    let selectDocumentEl = JS.Document.GetElementById("selectDocument")
                    while shouldWait <> -1 && shouldWait < 1000 do
                        do! Async.Sleep 20
                        [ for i = 0 to (documents.Length - 1) do
                            yield (selectDocumentEl?options?length <= i || (selectDocumentEl?options?item(i)?text |> string) = documents.[i].name)
                        ]
                        |> List.forall id
                        |> fun b -> shouldWait <- if b then -1 else shouldWait + 1
                    stateRefs.activeDocumentName.Value <- documents.Head.name
                | Failure msg -> JS.Alert msg
                | Error -> JS.Alert("Sorry, an error occurred")
            }
        match Cookies.Get "sessionGuid" with
        | Optional.Defined sessionGuid ->
            JS.Alert "sessionGuid found"
            async {
                let! rLogin = Server.loginWithSessionGuid sessionGuid
                match rLogin with
                | Ok user ->
                    setUser user
                    let parameters = Url.getParameters JS.Window.Location.Href
                    parameters
                    |> Map.tryFind "activeSidebarItems"
                    |> Option.iter (fun divs ->
                        stateRefs.activeSidebarItems.Value <-
                            (divs.Split [|','|] |> List.ofArray)
                    )

                    match parameters |> Map.tryFind "email", parameters |> Map.tryFind "confirmEmailGuid" with
                    | Some email, Some confirmEmailGuid ->
                        JS.Alert "email and guid parameters found"
                        let! rIsPasswordSet = Server.isPasswordSet ()
                        let! rIsEmailConfirmed = Server.isEmailConfirmed ()
                        match rIsPasswordSet, rIsEmailConfirmed with
                        | Ok true, Ok true ->
                            JS.Alert "true, true"
                            do! loadDocuments ()
                            do! loadSentApplications()
                        | Ok true, Ok false ->
                            JS.Alert "true, false"
                            let! rConfirmEmail = Server.setConfirmEmailGuidToNone email confirmEmailGuid
                            match rConfirmEmail with
                            | Ok () ->
                                do! loadDocuments ()
                                do! loadSentApplications()
                            | Failure msg -> JS.Alert msg
                            | Error -> JS.Alert "Sorry, an error occurred"
                        | Ok false, Ok true ->
                            JS.Alert "false, true"
                            stateRefs.activeSidebarItems.Value <- ["SetPasswordForFirstTime"]
                        | Ok false, Ok false ->
                            JS.Alert "false, false"
                            stateRefs.activeSidebarItems.Value <- ["SetPasswordForFirstTime"]
                            JS.Alert "hier!!jfds"
                        | Failure msg, _ -> JS.Alert msg
                        | _, Failure msg -> JS.Alert msg
                        | Error, _
                        | _, Error -> JS.Alert "Sorry, an error occurred"
                    | _ -> ()
                | Failure _ -> JS.Alert("Failed to log you in")
                | Error -> JS.Alert("Sorry, an error occurred")
            } |> Async.Start
        | Optional.Undefined ->
            JS.Alert "no sessionGuid found"
        let sentApplicationsTemplate =
            Templates.SentApplicationsTemplate()
                .SentApplications(stateRefs.sentApplications.View.DocSeqCached(fun (sentApplication : SentApplication) ->
                    Templates.SentApplication()
                        .Company(sentApplication.employer.company)
                        .Click(fun () ->
                            let modalEl = JS.Document.GetElementById("myModal")
                            let closeEl = JS.Document.GetElementsByClassName("close")?item(0)
                            state.Value <-
                                { state.Value with
                                    sentApplicationsModalValues =
                                        { state.Value.sentApplicationsModalValues with
                                            employer =
                                                { state.Value.employer with
                                                    company = sentApplication.employer.company
                                                    firstName = sentApplication.employer.firstName
                                                }
                                            statusHistory = sentApplication.statusHistory
                                        }
                                }
                            modalEl?style?display <- "block"
                            closeEl?onclick <-
                                (fun () ->
                                    modalEl?style?display <- "none";
                                )
                        )
                        .AppliedOn((fst sentApplication.statusHistory.[0]).ToShortDateString())
                        .Doc())
                )
                .SentApplicationsActive(Attr.DynamicClass "sidebarItemActive" stateRefs.activeSidebarItems.View (fun s -> not (s |> List.contains "SentApplications")))
                .Company(state.View.Map(fun s -> s.sentApplicationsModalValues.employer.company))
                .Doc()
        JS.Window.Onclick <-
            (fun evt ->
                let modalEl = JS.Document.GetElementById("myModal")
                if evt?target = modalEl && modalEl?style?display = "block"
                then modalEl?style?display <- "block";
            )

        let documentsAndFilesTemplate =
            Templates.DocumentsAndFilesTemplate()
                .DocumentsAndFilesActive(Attr.DynamicClass "sidebarItemActive" stateRefs.activeSidebarItems.View (fun s -> not (s |> List.contains "DocumentsAndFiles")))
                .DocumentPages(
                    stateRefs.pages.View.DocSeqCached(fun (page : Page) ->
                        match page with
                        | FilePage filePage ->
                            Templates.DocumentFile()
                                .Name(filePage.name)
                                //.IsActive(Attr.DynamicClass "isActive" state.View (fun (s : State) -> s.activeFileName = page.Name()))
                                //.Click(fun () -> stateRefs.activeFileName.Value <- filePage.name)
                                .Delete(fun _ ->
                                    stateRefs.pages.Value <- stateRefs.pages.Value |> List.removeFirst (fun page -> page.Name() = filePage.name)
                                    async {
                                        //let! _ = Server.saveAsNewDocument (currentDocument()) (state.Value.user.Values().id)
                                        return ()
                                    }
                                    |> Async.Start
                                )
                                .MoveUp(fun _ ->
                                    stateRefs.pages.Value <- stateRefs.pages.Value |> List.moveUp (fun p -> p.Name() = filePage.name)
                                    async {
                                        //let! _ = Server.saveAsNewDocument (currentDocument()) (state.Value.user.Values().id)
                                        return ()
                                    }
                                    |> Async.Start
                                )
                                .MoveDown(fun _ ->
                                    stateRefs.pages.Value <- stateRefs.pages.Value |> List.moveDown (fun p -> p.Name() = filePage.name)
                                    async {
                                        //let! _ = Server.saveAsNewDocument (currentDocument()) (state.Value.user.Values().id)
                                        return ()
                                    }
                                    |> Async.Start
                                )
                                .Download(fun () ->
                                    async {
                                        JS.Alert filePage.name
                                        //let! _ = ajax "GET" "/Download?filename=hallo.png" ""
                                        let! rLinkGuid = Server.createDownloadLink filePage.path filePage.name
                                        match rLinkGuid with
                                        | Ok linkGuid ->
                                            //let! _ = ajax "GET" ("/Download?linkGuid=" + linkGuid) ""
                                            JS.Window.Location.Href <- ("/Download?linkGuid=" + linkGuid)
                                            ()
                                        | Failure msg -> JS.Alert msg
                                        | Error -> JS.Alert "Sorry, an error occurred"
                                    } |> Async.Start
                                )
                                .Doc()
                        | HtmlPage htmlPage ->
                            Doc.Empty
                        )
                )
                .UploadFile(fun el ev ->
                    ev.PreventDefault()
                    ev.StopPropagation()
                    async {
                        let formData : FormData = JS.Eval("""new FormData(document.getElementById("formId"));""") :?> FormData
                        let! rUserId = Server.getCurrentUserId ()
                        match rUserId with
                        | Ok userId ->
                            formData.Append("userId", string <| userId)
                            formData.Append("documentId", string (state.Value.documents |> List.find (fun x -> x.name = state.Value.activeDocumentName) |> fun x -> x.id))
                            let! _ = ajax "POST" "/Upload" formData
                            ()
                        | Failure msg -> JS.Alert(msg)
                        | Error -> JS.Alert("Sorry, an error occurred")
                    } |> Async.Start
                )
                .SelectDocument(createSelect "selectDocument" (stateRefs.documents.View.Map(fun ds -> ds |> (List.map (fun (x : Document) -> x.name)))) stateRefs.activeDocumentName
                )
                .Doc()
        let applyNowTemplate =
            Templates.ApplyNowTemplate()
                //.Change(fun () ->
                //    files.Value <- ([{ size = 88; name = "Hallo" }; {size=0; name="Welt"}])
                //)
                //.Change2(fun () ->
                //    files.Value <- ([{ size = 88; name = "abc" }; {size = 0; name="def"}; {size = 0; name="ghi"}])
                //)
                .ApplyNowActive(Attr.DynamicClass "sidebarItemActive" stateRefs.activeSidebarItems.View (fun s -> not (s |>  List.contains "ApplyNow")))
                .UserEmail(
                    Templates.InputField().Id("applyNowUserEmail").LabelText("Your email").Var(stateRefs.user.email).Doc()
                    (*
                    match state.Value.user with
                    | Guest _ ->
                        Templates.InputField().Id("applyNowUserEmail").LabelText("Your email").Var(stateRefs.user.email).Doc()
                    | LoggedInUser _ ->
                        Doc.Empty)
                    *)
                )
                .JobName(Templates.InputField().Id("jobName").LabelText("ApplyAs").Var(stateRefs.jobName).Doc())
                .Company(Templates.InputField().Id("employerCompany").LabelText("Company").Var(stateRefs.employer.company).Doc())
                .Gender(createRadioButton "Gender" [ ("Male", Gender.Male); ("Female", Gender.Female); ("Unknown", Gender.Unknown) ] stateRefs.employer.gender)
                .Degree(Templates.InputField().Id("employerDegree").LabelText("Degree").Var(stateRefs.employer.degree).Doc())
                .FirstName(Templates.InputField().Id("firstName").LabelText("First name").Var(stateRefs.employer.firstName).Doc())
                .LastName(Templates.InputField().Id("lastName").LabelText("Last name").Var(stateRefs.employer.lastName).Doc())
                .Street(Templates.InputField().Id("employerStreet").LabelText("Street").Var(stateRefs.employer.street).Doc())
                .Postcode(Templates.InputField().Id("employerPostcode").LabelText("Postcode").Var(stateRefs.employer.postcode).Doc())
                .City(Templates.InputField().Id("employerCity").LabelText("City").Var(stateRefs.employer.city).Doc())
                .Email(Templates.InputField().Id("employerEmail").LabelText("Email").Var(stateRefs.employer.email).Doc())
                .Phone(Templates.InputField().Id("employerPhone").LabelText("Phone").Var(stateRefs.employer.phone).Doc())
                .MobilePhone(Templates.InputField().Id("employerMobilePhone").LabelText("MobilePhone").Var(stateRefs.employer.mobilePhone).Doc())
                .ApplyNowClick(fun () ->
                    async {
                        let! rApplyNow = Server.applyNow (state.Value.user) state.Value.employer (currentDocument())
                        match rApplyNow with
                        | Ok _ -> JS.Alert("Application would have been sent")
                        | Failure msg -> JS.Alert(msg)
                        | Error -> JS.Alert("Sorry, your application could not be sent.")
                    } |> Async.Start
                )
                .Doc()
        let userValuesTemplate =
            Templates.UserValuesTemplate()
                .UserValuesActive(Attr.DynamicClass "sidebarItemActive" stateRefs.activeSidebarItems.View (fun s -> not (s |>  List.contains "UserValues")))
                .Gender(createRadioButton "Gender" [ ("Male", Gender.Male); ("Female", Gender.Female) ] stateRefs.user.gender)
                .Degree(Templates.InputField().Id("userDegree").LabelText("Degree").Var(stateRefs.user.degree).Doc())
                .FirstName(Templates.InputField().Id("userFirstName").LabelText("First name").Var(stateRefs.user.firstName).Doc())
                .LastName(Templates.InputField().Id("userLastName").LabelText("Last name").Var(stateRefs.user.lastName).Doc())
                .Street(Templates.InputField().Id("userStreet").LabelText("Street").Var(stateRefs.user.street).Doc())
                .Postcode(Templates.InputField().Id("userPostcode").LabelText("Postcode").Var(stateRefs.user.postcode).Doc())
                .City(Templates.InputField().Id("userCity").LabelText("City").Var(stateRefs.user.city).Doc())
                .Email(Templates.InputField().Id("userEmail").LabelText("Email").Var(stateRefs.user.email).Doc())
                .Phone(Templates.InputField().Id("userPhone").LabelText("Phone").Var(stateRefs.user.phone).Doc())
                .MobilePhone(Templates.InputField().Id("userMobilePhone").LabelText("MobilePhone").Var(stateRefs.user.mobilePhone).Doc())
                .Doc()
        let customVariablesTemplate = 
            Templates.CustomVariablesTemplate()
                .CustomVariablesActive(Attr.DynamicClass "sidebarItemActive" stateRefs.activeSidebarItems.View (fun s -> not (s |>  List.contains "CustomVariables")))
                .CustomVariables(
                    stateRefs.customVariables.View.DocSeqCached(fun (customVariable : CustomVariable) ->
                        Templates.CustomVariable()
                            .Text(
                                stateRefs.customVariablesSaved.Lens
                                    (fun s -> s |> List.pick (fun x -> if x.index = customVariable.index then Some x.text else None))
                                    (fun s v -> s |> List.map (fun x -> if x.index = customVariable.index then { x with text = v} else x))
                            )
                            .Doc()
                    )
                )
                .AddVariableClick(fun () ->
                    stateRefs.customVariables.Value <- stateRefs.customVariables.Value @ [ {index = stateRefs.customVariables.Value.Length; text = ""} ]
                    stateRefs.customVariablesSaved.Value <- stateRefs.customVariablesSaved.Value @ [ {index = stateRefs.customVariablesSaved.Value.Length; text = ""} ]
                )
                .Doc()
        let emailTemplate =
            Templates.EmailTemplate()
                .EmailActive(Attr.DynamicClass "sidebarItemActive" stateRefs.activeSidebarItems.View (fun s -> not (s |>  List.contains "Email")))
                .Subject(Templates.InputField().Id("emailSubject").LabelText("Subject").Var(stateRefs.email.subject).Doc())
                .Body(Templates.TextareaField().Id("emailBody").LabelText("Body").MinHeight("400px").Var(stateRefs.email.body).Doc())
                .Doc()
        let loginTemplate =
            Templates.LoginTemplate()
                .Email(Templates.InputField().Id("loginEmail").LabelText("Email").Var(stateRefs.login.email).Doc())
                .Password(Templates.PasswordField().Id("loginPassword").LabelText("Password").Var(stateRefs.login.password).Doc())
                .Submit(fun () ->
                    async {
                        let! rLogin = Server.loginWithEmailAndPassword state.Value.login.email state.Value.login.password
                        match rLogin with
                        | Ok (sessionGuid, user) ->
                            Cookies.Set("sessionGuid", sessionGuid)
                            setUser user
                            do! loadDocuments ()
                            do! loadSentApplications ()
                        | Failure msg -> JS.Alert(msg)
                        | _ -> JS.Alert("Sorry an error occurred.")
                    } |> Async.Start
                )
                .LoginActive(Attr.DynamicClass "sidebarItemActive" stateRefs.activeSidebarItems.View (fun s -> not (s |>  List.contains "Login")))
                .Doc()
        let registerTemplate =
            Templates.RegisterTemplate()
                .Email(Templates.InputField().Id("registerEmail").LabelText("Email").Var(stateRefs.register.email).Doc())
                .Password(Templates.PasswordField().Id("registerPassword").LabelText("Password").Var(stateRefs.register.password).Doc())
                .Submit(fun () ->
                    async {
                        let! rRegister = Server.register state.Value.register.email state.Value.register.password
                        match rRegister with
                        | Ok (sessionGuid, user) ->
                            Cookies.Set("sessionGuid", sessionGuid)
                            JS.Alert "sessionGuid set!"
                            setUser user
                        | Failure msg -> JS.Alert(msg)
                        | Error -> JS.Alert("Sorry, an error occurred")
                    } |> Async.Start
                )
                .RegisterActive(Attr.DynamicClass "sidebarItemActive" stateRefs.activeSidebarItems.View (fun s -> not (s |>  List.contains "Register")))
                .Doc()
        let changePasswordTemplate =
            Templates.ChangePasswordTemplate()
                .Password(Templates.PasswordField().Id("changePasswordPassword").LabelText("New password").Var(stateRefs.changePassword.password).Doc())
                .Submit(fun () ->
                    async {
                        let! rChangePassword = Server.changePassword state.Value.changePassword.password
                        match rChangePassword with
                        | Ok () ->
                            JS.Alert("Changing password was successful")
                        | Failure msg -> JS.Alert(msg)
                        | Error -> JS.Alert("Sorry, an error occurred")
                    } |> Async.Start
                )
                .ChangePasswordActive(Attr.DynamicClass "sidebarItemActive" stateRefs.activeSidebarItems.View (fun s -> not (s |>  List.contains "ChangePassword")))
                .Doc()
        let changeEmailTemplate =
            Templates.ChangeEmailTemplate()
                .Email(Templates.InputField().Id("changeEmailEmail").LabelText("New email").Var(stateRefs.user.email).Doc())
                .Submit(fun () ->
                    async {
                        JS.Alert("unimplemented")
                    } |> Async.Start
                )
                .ChangeEmailActive(Attr.DynamicClass "sidebarItemActive" stateRefs.activeSidebarItems.View (fun s -> not (s |>  List.contains "ChangeEmail")))
                .Doc()
        let logoutTemplate =
            Templates.LogoutTemplate()
                .Logout(fun () ->
                    async {
                        let! rLogoutResult = Server.logout()
                        match rLogoutResult with
                        | Ok () -> ()
                        | Failure msg -> JS.Alert msg
                        | Error -> JS.Alert("Sorry, an error occurred")
                    } |> Async.Start
                )
                .LogoutActive(Attr.DynamicClass "sidebarItemActive" stateRefs.activeSidebarItems.View (fun s -> not (s |>  List.contains "Logout")))
                .Doc()
        let resendConfirmationEmailEmailTemplate =
            Templates.ResendConfirmEmailEmailTemplate()
                .Submit(fun () ->
                    async {
                        let! rSendConfirmationEmail = Server.sendConfirmationEmailEmail ()
                        match rSendConfirmationEmail with
                        | Ok () -> ()
                        | Failure msg -> JS.Alert msg
                        | Error -> JS.Alert "Sorry, an error occurred"
                    } |> Async.Start
                )
                .ResendConfirmEmailEmailActive(Attr.DynamicClass "sidebarItemActive" stateRefs.activeSidebarItems.View (fun s -> not (s |>  List.contains "ResendConfirmEmailEmail")))
                .Email(stateRefs.user.email.View)
                .Doc()
        let forgotPasswordTemplate =
            Templates.ForgotPasswordTemplate()
                .Email(Templates.InputField().Id("resendPasswordTo").LabelText("Email").Var(stateRefs.register.password).Doc())
                .Submit(fun () ->
                    async {
                        let! rForgotPassword = Server.register state.Value.register.email state.Value.register.password
                        match rForgotPassword with
                        | Ok (sessionGuid, user) ->
                            JS.Alert("We have sent you a link to your email address. Please visit this link to change your password.")
                        | Failure msg -> JS.Alert(msg)
                        | Error -> JS.Alert("Sorry, an error occurred")
                    } |> Async.Start
                )
                .ForgotPasswordActive(Attr.DynamicClass "sidebarItemActive" stateRefs.activeSidebarItems.View (fun s -> not (s |>  List.contains "ForgotPassword")))
                .Doc()
        let setPasswordForFirstTimeTemplate =
            Templates.SetPasswordForFirstTimeTemplate()
                .Password(Templates.PasswordField().Id("setPasswordForFirstTimePassword").LabelText("Password").Var(stateRefs.changePassword.password).Doc())
                .Submit(fun () ->
                    async {
                        try
                            if state.Value.changePassword.password <> ""
                            then
                                let parameters = Url.getParameters JS.Window.Location.Href
                                match parameters |> Map.tryFind "email", parameters |> Map.tryFind "confirmEmailGuid" with
                                | Some urlEmail, Some urlConfirmEmailGuid ->
                                    let! rConfirmEmail, rLogin, rChangePassword =
                                        Server.confirmEmail urlEmail urlConfirmEmailGuid state.Value.changePassword.password
                                    match rConfirmEmail, rLogin, rChangePassword with
                                    | Ok _, Ok (sessionGuid, user), Ok _ ->
                                        JS.Alert "email confirmed!"
                                        Cookies.Set("sessionGuid", sessionGuid)
                                        setUser user
                                        do! loadDocuments ()
                                        do! loadSentApplications ()
                                    | _ -> JS.Alert "Sorry, an error occurred"
                                | _ ->
                                    ()
                        with
                        | _ ->
                            JS.Alert("Sorry, an error occurred.")
                    } |> Async.Start
                )
                .SetPasswordForFirstTimeActive(Attr.DynamicClass "sidebarItemActive" stateRefs.activeSidebarItems.View (fun s -> not (s |>  List.contains "SetPasswordForFirstTime")))
                .Doc()
        MainTemplate()
            .Title("Bewerbungsspam")
            .Body(
                Doc.Concat
                  [ sentApplicationsTemplate
                    documentsAndFilesTemplate
                    applyNowTemplate
                    emailTemplate
                    userValuesTemplate
                    customVariablesTemplate
                    loginTemplate
                    registerTemplate
                    logoutTemplate
                    changePasswordTemplate
                    changeEmailTemplate
                    forgotPasswordTemplate
                    setPasswordForFirstTimeTemplate
                    resendConfirmationEmailEmailTemplate
                  ]
            )
            .MainbarLoginDiv(Doc.EmbedView stateRefs.mainbarLoginDiv.View)
            .SidebarMenuApplyNowClick(
                fun () -> stateRefs.activeSidebarItems.Value <- ["ApplyNow"]
            )
            .SidebarMenuEmailClick(
                fun () -> stateRefs.activeSidebarItems.Value <- ["Email"]
            )
            .SidebarMenuSentApplicationsClick(
                fun () -> stateRefs.activeSidebarItems.Value <- ["SentApplications"]
            )
            .SidebarMenuDocumentsAndFilesClick(
                fun () -> stateRefs.activeSidebarItems.Value <- ["DocumentsAndFiles"]
            )
            .SidebarMenuUserValuesClick(
                fun () -> stateRefs.activeSidebarItems.Value <- ["UserValues"]
            )
            .SidebarMenuCustomVariablesClick(
                fun () -> stateRefs.activeSidebarItems.Value <- ["CustomVariables"]
            )
            .Doc()
