namespace JobApplicationSpam

open WebSharper

[<JavaScript>]
module Types =
    open WebSharper.Sitelets.Http
    open System.Web
    open FSharp.Configuration
    open System
    open System.Configuration
    open System.IO
    open FSharp.Data.Sql

    type Settings = AppSettings<"Web.config">

    type DB =
        SqlDataProvider<
            DatabaseVendor = FSharp.Data.Sql.Common.DatabaseProviderTypes.POSTGRESQL,
            ConnectionString = ConnectionStrings.Procuction,
            ResolutionPath = "bin",
            IndividualsAmount = 1000,
            UseOptionTypes = true>
    
    type FilePage =
        { name : string
          size : int
          path : string
//          requiredVariables : list<string>
        }

    type HtmlPage =
        { name : string }
    type Page =
    | FilePage of FilePage
    | HtmlPage of HtmlPage
    with
        member this.Name() =
            match this with
            | FilePage filePage -> filePage.name
            | HtmlPage htmlPage -> htmlPage.name
    
    type CustomVariable =
        { index : int
          text : string
        }

    type Document =
        { name : string
          pages : list<Page>
          id : int
          customVariables : list<CustomVariable>
          jobName : string
          emailSubject : string
          emailBody : string
        }
    type Gender = 
        | Male
        | Female
        | Unknown
        with
            override this.ToString() =
                match this with
                | Male -> "m"
                | Female -> "f"
                | Unknown -> "u"
            static member FromString(s) =
                match s with
                | "m" -> Gender.Male
                | "f" -> Gender.Female
                | "u" -> Gender.Unknown
                | x -> failwith ("Gender not found: " + x)
    type Login =
        { email : string
          password : string
        }
    type Register =
        { email : string
          password : string
        }
    type ChangePassword =
        { password : string
        }
    type ForgotPassword =
        { email : string
        }
    type ChangeEmail =
        { email : string
        }
    type UserValues =
        { gender : Gender
          degree : string
          firstName : string
          lastName : string
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
          firstName = ""
          lastName = ""
          street = ""
          postcode = ""
          city = ""
          email = ""
          phone = ""
          mobilePhone = ""
        }
    type FileUpload =
            { [<FormData>] userId : int
              [<FormData>] documentId : int
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
    type User = 
        | LoggedInUser of UserValues
        | Guest of UserValues
        with
            member this.Values() =
                match this with
                | LoggedInUser userValues -> userValues
                | Guest userValues -> userValues
            member this.Values(newUserValues) =
                match this with
                | LoggedInUser _ -> LoggedInUser newUserValues
                | Guest _ -> Guest newUserValues

    type SentApplicationStatus =
    | NotYetSent = 1
    | WaitingForReply = 2
    | ApplicationDenied = 3
    | AppointmentForJobInterview = 4
    | NotHired = 5
    | Hired = 6

    type SentApplication =
        { employer : Employer
          userValues : UserValues
          emailSubject : string
          emailBody : string
          jobName : string
          customVariables : list<CustomVariable>
          statusHistory : list<DateTime * int>
        }
    type EmptyTextTagAction =
    | Replace
    | Ignore

    [<CompilationRepresentation(CompilationRepresentationFlags.UseNullAsTrueValue)>]
    type Result<'a> =
    | Ok of 'a
    | Failure of string
    | Error
    
    let iter f r =
        match r with
        | Ok v ->
            f r
        | x -> x

