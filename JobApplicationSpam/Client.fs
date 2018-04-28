namespace JobApplicationSpam

open WebSharper
open WebSharper.JavaScript
open WebSharper.JQuery
open WebSharper.UI.Next
open WebSharper.UI.Next.Client
open WebSharper.UI.Next.Templating
open WebSharper.UI.Next.Html

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

[<JavaScript>]
module Client =
    type SentApplicationsTemplate = Templating.Template<"templates/SentApplications.html">
    type UserValuesTemplate = Templating.Template<"templates/UserValues.html">
    type ApplyNowTemplate = Templating.Template<"templates/ApplyNow.html">
    type EmailTemplate = Templating.Template<"templates/Email.html">
    type Templates = Templating.Template<"templates/Templates.html">

    type DocumentFile =
        { name : string
          size : int
        }
    type Document =
        { name : string
          files : list<DocumentFile>
        }
    
    type Gender = 
        | Male
        | Female
        | Unknown
    type UserValues =
        {
          gender : Gender
          degree : string
          name : string
          street : string
          postcode : string
          city : string
          email : string
          phone : string
          mobilePhone : string
        }
    let emptyUserValues =
        { gender = Gender.Unknown
          degree = ""
          name = ""
          street = ""
          postcode = ""
          city = ""
          email = ""
          phone = ""
          mobilePhone = ""
        }
    type User = 
        | LoggedInUser of UserValues
        | Guest of UserValues
        with
            member this.Values() =
                match this with
                | LoggedInUser userValues -> userValues
                | Guest userValues -> userValues
            member this.Values(values) =
                match this with
                | LoggedInUser _ -> LoggedInUser values
                | Guest _ -> Guest values

    type Email =
        { subject : string
          body : string
        }
    type Employer =
        { company : string
          street : string
          postcode : string
          city : string
          gender : Gender
          degree : string
          firstName : string
          lastName : string
          email : string
          phone : string
          mobilePhone : string
        }
    let emptyEmployer =
        { company = ""
          street = ""
          postcode = ""
          city = ""
          gender = Gender.Unknown
          degree = ""
          firstName = ""
          lastName = ""
          email = ""
          phone = ""
          mobilePhone = ""
        }
    type State =
        { documents : list<Document>
          activeFileName : string
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
        }
    let state =
        Var.Create
            { documents = []
              activeFileName = ""
              employer = emptyEmployer
              user = Guest emptyUserValues
              email = { subject = ""; body = "" }
            }
    let stateRefs =
        { documents = state.Lens (fun s -> s.documents) (fun s v -> { s with documents = v })
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

    let files = 
        Var.Create [ { size = 3; name = "John"}; {size = 34; name= "Paul" }]

    [<JavaScript>]
    let Main () =
        let newName = Var.Create ""
        let applyNowTemplate =
            ApplyNowTemplate.ApplyNow()
                .DocumentFiles(
                    files.View.DocSeqCached(fun (file : DocumentFile) ->
                        ApplyNowTemplate.DocumentFile()
                            .Name(file.name)
                            .Click(fun () ->
                                JS.Alert(file.name)
                            )
                            .Delete(fun _ ->
                                files.Value <- files.Value |> List.removeFirst (fun x -> x.name = file.name))
                            .MoveUp(fun _ ->
                                files.Value <- List.moveUp (fun x -> x.name = file.name) files.Value)
                            .MoveDown(fun _ ->
                                files.Value <- List.moveDown (fun x -> x.name = file.name) files.Value)
                            .Doc()
                    )
                )
                .Change(fun _ ->
                    files.Value <- ([{ size = 88; name = "Hallo" }; {size=0; name="Welt"}])
                )
                .Change2(fun _ ->
                    files.Value <- ([{ size = 88; name = "abc" }; {size = 0; name="def"}; {size = 0; name="ghi"}])
                )
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

