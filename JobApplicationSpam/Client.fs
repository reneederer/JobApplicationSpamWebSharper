namespace JobApplicationSpam

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI.Next
open WebSharper.UI.Next.Client
open WebSharper.UI.Next.Html
open Types

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

[<JavaScript>]
module Client =
    type SentApplicationsTemplate = Templating.Template<"templates/SentApplications.html">
    type UserValuesTemplate = Templating.Template<"templates/UserValues.html">
    type ApplyNowTemplate = Templating.Template<"templates/ApplyNow.html">
    type EmailTemplate = Templating.Template<"templates/Email.html">
    type Templates = Templating.Template<"templates/Templates.html">

    type State =
        { documents : list<Document>
          activeFileName : string
          activeDocumentName : string
          employer : Employer
          user : User
          email : Email
        }
    type UserRefs =
        { gender : IRef<Gender>
          degree : IRef<string>
          name : IRef<string>
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
    type StateRefs =
        { documents : IRef<list<Document>>
          user : UserRefs
          employer : EmployerRefs
          email : EmailRefs
          activeFileName : IRef<string>
          activeDocumentName : IRef<string>
          files : IRef<list<DocumentFile>>
        }
    let state : Var<State> =
        Var.Create
            { documents =
                [
                  { files = [{name = "AAA"; size = 0}];
                    name="Doc 1"
                  }
                  { files = [{name = "XX"; size = 0}; {name = "YY"; size = 0}; {name = "ZZ"; size = 0}];
                    name="Doc 2"
                  }
                  { files = [{name = "D"; size = 0}; {name = "E"; size = 0}; {name = "F"; size = 0}];
                    name="Doc 3"
                  }
                  { files = [{name = "u"; size = 0}; {name = "v"; size = 0}];
                    name="Doc 4"
                  }
                ]
              activeFileName = ""
              activeDocumentName = "Doc 1"
              employer = emptyEmployer
              user = Guest emptyUserValues
              email = { subject = ""; body = "" }
            }
    let stateRefs =
        { documents = state.Lens (fun s -> s.documents) (fun s v -> { s with documents = v })
          files =
              state.Lens
                  (fun s ->
                      let documentIndex = s.documents |> List.findIndex (fun x -> x.name = s.activeDocumentName)
                      let xs = List.mapAtOrDefault documentIndex (fun (x : Document) -> x.files) [] s.documents
                      xs)
                  (fun s v ->
                      let documentIndex = s.documents |> List.findIndex (fun x -> x.name = s.activeDocumentName)
                      if List.length s.documents <= documentIndex
                      then s
                      else { s with documents = List.replace documentIndex {s.documents.[documentIndex] with files = v} s.documents })
          user =
            { gender = Var.Create Gender.Male
              degree = state.Lens (fun s -> s.user.Values().degree) (fun s v -> { s with user = s.user.Values({ s.user.Values() with degree = v })})
              name = state.Lens (fun s -> s.user.Values().name) (fun s v -> { s with user = s.user.Values({ s.user.Values() with name = v })})
              street = state.Lens (fun s -> s.user.Values().street) (fun s v -> { s with user = s.user.Values({ s.user.Values() with street = v })})
              postcode = state.Lens (fun s -> s.user.Values().postcode) (fun s v -> { s with user = s.user.Values({ s.user.Values() with postcode = v })})
              city = state.Lens (fun s -> s.user.Values().city) (fun s v -> { s with user = s.user.Values({ s.user.Values() with city = v })})
              email = state.Lens (fun s -> s.user.Values().email) (fun s v -> { s with user = s.user.Values({ s.user.Values() with email = v })})
              phone = state.Lens (fun s -> s.user.Values().phone) (fun s v -> { s with user = s.user.Values({ s.user.Values() with phone = v })})
              mobilePhone = state.Lens (fun s -> s.user.Values().mobilePhone) (fun s v -> { s with user = s.user.Values({ s.user.Values() with mobilePhone = v })})
            }
          employer =
            { company = state.Lens (fun s -> s.employer.company) (fun s v -> { s with employer = { s.employer with company = v } })
              gender = Var.Create Gender.Male
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
            { subject = state.Lens (fun s -> s.email.subject) (fun s v -> { s with email = { s.email with subject = v }})
              body = state.Lens (fun s -> s.email.body) (fun s v -> { s with email = { s.email with body = v }})
            }
          activeFileName = state.Lens (fun s -> s.activeFileName) (fun s v -> { s with activeFileName = v })
          activeDocumentName = state.Lens (fun s -> s.activeDocumentName) (fun s v -> { s with activeDocumentName = v })
        }
    
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
    
    let createSelect  items ref =
        Doc.SelectDyn
            []
            id
            items
            ref

    let Main () =
        let applyNowTemplate =
            ApplyNowTemplate.ApplyNow()
                .DocumentFiles(
                        stateRefs.files.View.DocSeqCached(fun (file : DocumentFile) ->
                                ApplyNowTemplate.DocumentFile()
                                    .Name(file.name)
                                    .IsActive(Attr.DynamicClass "isActive" state.View (fun (s : State) -> s.activeFileName = file.name))
                                    //.MoveDownVisible(Attr.DynamicClass "vis" state.View (fun (s : State) -> if s.documents.Length = 0 || (s.documents.[0].files |> List.length = 0) then false else (s.documents.[0].files |> List.last |> (fun x -> x.name)) = file.name))
                                    .Click(fun () ->
                                        stateRefs.activeFileName.Value <- file.name
                                    )
                                    .Delete(fun _ ->
                                        stateRefs.files.Value <- stateRefs.files.Value |> List.removeFirst (fun x -> x.name = file.name))
                                    .MoveUp(fun _ ->
                                        stateRefs.files.Value <- stateRefs.files.Value |> List.moveUp (fun x -> x.name = file.name))
                                    .MoveDown(fun _ ->
                                        stateRefs.files.Value <- stateRefs.files.Value |> List.moveDown (fun x -> x.name = file.name))
                                    .Doc()
                            )
                )
                .SelectDocument(createSelect (stateRefs.documents.View.Map(fun ds -> ds |> (List.map (fun (x : Document) -> x.name)))) stateRefs.activeDocumentName
                )
                //.Change(fun () ->
                //    files.Value <- ([{ size = 88; name = "Hallo" }; {size=0; name="Welt"}])
                //)
                //.Change2(fun () ->
                //    files.Value <- ([{ size = 88; name = "abc" }; {size = 0; name="def"}; {size = 0; name="ghi"}])
                //)
                .Gender(createRadioButton "Gender" [ ("Male", Gender.Male); ("Female", Gender.Female); ("Unknown", Gender.Unknown) ] stateRefs.employer.gender)
                .Degree(Templates.InputField().Id("employerDegree").LabelText("Degree").Var(stateRefs.employer.degree).Doc())
                .FirstName(Templates.InputField().Id("employerCity").LabelText("City").Var(stateRefs.employer.firstName).Doc())
                .LastName(Templates.InputField().Id("employerCity").LabelText("City").Var(stateRefs.employer.lastName).Doc())
                .Street(Templates.InputField().Id("employerStreet").LabelText("Street").Var(stateRefs.employer.street).Doc())
                .Postcode(Templates.InputField().Id("employerPostcode").LabelText("Postcode").Var(stateRefs.employer.postcode).Doc())
                .City(Templates.InputField().Id("employerCity").LabelText("City").Var(stateRefs.employer.city).Doc())
                .Email(Templates.InputField().Id("employerEmail").LabelText("Email").Var(stateRefs.employer.email).Doc())
                .Phone(Templates.InputField().Id("employerPhone").LabelText("Phone").Var(stateRefs.employer.phone).Doc())
                .MobilePhone(Templates.InputField().Id("employerMobilePhone").LabelText("MobilePhone").Var(stateRefs.user.mobilePhone).Doc())
                .Doc()
        let userValuesTemplate =
            UserValuesTemplate.UserValues()
                .Gender(createRadioButton "Gender" [ ("Male", Gender.Male); ("Female", Gender.Female) ] stateRefs.user.gender)
                .Degree(Templates.InputField().Id("userDegree").LabelText("Degree").Var(stateRefs.user.degree).Doc())
                .Name(Templates.InputField().Id("userCity").LabelText("City").Var(stateRefs.user.name).Doc())
                .Street(Templates.InputField().Id("userStreet").LabelText("Street").Var(stateRefs.user.street).Doc())
                .Postcode(Templates.InputField().Id("userPostcode").LabelText("Postcode").Var(stateRefs.user.postcode).Doc())
                .City(Templates.InputField().Id("userCity").LabelText("City").Var(stateRefs.user.city).Doc())
                .Email(Templates.InputField().Id("userEmail").LabelText("Email").Var(stateRefs.user.email).Doc())
                .Phone(Templates.InputField().Id("userPhone").LabelText("Phone").Var(stateRefs.user.phone).Doc())
                .MobilePhone(Templates.InputField().Id("userMobilePhone").LabelText("MobilePhone").Var(stateRefs.user.mobilePhone).Doc())
                .Doc()
        let emailTemplate =
            EmailTemplate.Email()
                .Subject(Templates.InputField().Id("emailSubject").LabelText("Subject").Var(stateRefs.email.subject).Doc())
                .Body(Templates.TextareaField().Id("emailBody").LabelText("Body").MinHeight("400px").Var(stateRefs.email.body).Doc())
                .Doc()
        Doc.Concat [applyNowTemplate; emailTemplate; userValuesTemplate]

