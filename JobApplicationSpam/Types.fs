namespace JobApplicationSpam

open WebSharper
open WebSharper.Sitelets
open WebSharper.UI.Next
open WebSharper.UI.Next.Server

[<JavaScript>]
module Types =
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
