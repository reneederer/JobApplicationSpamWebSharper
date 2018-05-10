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
            ConnectionString = "Server=localhost; Port=5432; User Id=spam; Password=Steinmetzstr9!@#$; Database=jobapplicationspam; Enlist=true",
            ResolutionPath = "bin",
            IndividualsAmount = 1000,
            UseOptionTypes = true>

    type FilePage =
        { name : string
          size : int
          path : string
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
    type Document =
        { name : string
          pages : list<Page>
          id : int
          customVariables : string
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
        { id : int
          gender : Gender
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
        { id = 0
          gender = Gender.Unknown
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
    type UserId = int
    type User = 
        | LoggedInUser of UserValues
        | Guest of UserValues
        with
            member this.Values() =
                match this with
                | LoggedInUser userValues -> userValues
                | Guest userValues -> userValues
            member this.Values(userValues) =
                match this with
                | LoggedInUser userValues -> LoggedInUser userValues
                | Guest userValues -> Guest userValues

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
          customVariables : string
          statusHistory : list<DateTime * int>
        }
    type Result<'a> =
    | Ok of 'a
    | Failure of string
    | Error

    type EmptyTextTagAction =
    | Replace
    | Ignore

