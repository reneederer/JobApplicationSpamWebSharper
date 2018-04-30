namespace JobApplicationSpam

open WebSharper
open WebSharper.Sitelets
open WebSharper.UI.Next
open WebSharper.UI.Next.Server

[<JavaScript>]
module Types =
    type FilePage =
        { name : string
          size : int
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
                | LoggedInUser _ -> LoggedInUser userValues
                | Guest _ -> Guest userValues

    type Result<'a> =
    | Ok of 'a
    | Failure of string
    | Error
