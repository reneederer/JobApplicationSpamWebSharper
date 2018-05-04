namespace JobApplicationSpam

open FsUnit.Xunit
open Server.Internal
open Types
open System.Transactions

open System
open Xunit

module Server = Server.Internal

type InitTest () =
    member this.dbContext =
        DB.GetDataContext(
            { FSharp.Data.Sql.Transactions.IsolationLevel = FSharp.Data.Sql.Transactions.IsolationLevel.ReadCommitted;
              FSharp.Data.Sql.Transactions.Timeout = TimeSpan.FromSeconds 900.}, 900)
    member val scope =
        new TransactionScope(TransactionScopeOption.Required, TransactionOptions(IsolationLevel = IsolationLevel.ReadCommitted)) with get, set
    interface IDisposable with
        member this.Dispose() =
            Server.getUserSession().Logout() |> Async.RunSynchronously
            this.scope <-
                new TransactionScope(TransactionScopeOption.Required, TransactionOptions(IsolationLevel = IsolationLevel.ReadCommitted))

type MyTests ()  =
    inherit InitTest()
    [<Fact>]
    member this.``login with correct email and password and confirmed email should set the loggedInUser to (Some userId) and return a LoggedInUser``() =
        let actual =
            match loginWithEmailAndPassword' "rene.ederer.nbg@gmail.com" "1234" (DB.GetDataContext()) with
            | Ok (sessionGuid, user) ->
                sessionGuid |> should not' (equal "")
                Ok ("", user)
            | v -> v
                
        let expected =
            Ok
              ( "",
                LoggedInUser
                  { gender = Gender.Male
                    degree = ""
                    firstName = "Rene"
                    lastName = "Ederer"
                    street = "Raabstr. 24A"
                    postcode = "90429"
                    city = "Nuernberg"
                    email = "rene.ederer.nbg@gmail.com"
                    phone = "kein Telefon"
                    mobilePhone = "kein Handy"
                  }
              )
        Server.getUserSession().GetLoggedInUser() |> Async.RunSynchronously |> should equal (Some "1")
        actual |> should equal expected

    [<Fact>]
    member this.``register with nonexisting email should login the user and return a Guest``() =
        let actual =
            match register' "nonexisting@email.com" "password" this.dbContext with
            | Ok (sessionGuid, user) ->
                sessionGuid |> should not' (equal "")
                Ok ("", user)
            | v -> v
                
        let expected =
            Ok
              ( "",
                Guest
                  { gender = Gender.Unknown
                    degree = ""
                    firstName = ""
                    lastName = ""
                    street = ""
                    postcode = ""
                    city = ""
                    email = "nonexisting@email.com"
                    phone = ""
                    mobilePhone = ""
                  }
              )
        //Server.getUserSession().GetLoggedInUser() |> Async.RunSynchronously |> should equal (Some "3")
        actual |> should equal expected

    [<Fact>]
    member this.``login with correct email and password and unconfirmed email should set the loggedInUser to (Some userId) and return a Guest``() =
        let actual =
            match loginWithEmailAndPassword' "helmut@goerke.de" "1" this.dbContext with
            | Ok (sessionGuid, user) ->
                sessionGuid |> should not' (equal "")
                Ok ("", user)
            | v -> v
                
        let expected =
            Ok
              ( "",
                Guest
                  { gender = Gender.Male
                    degree = ""
                    firstName = "Helmut"
                    lastName = "Goerke"
                    street = "Raabstr. 24A"
                    postcode = "90429"
                    city = "Nuernberg"
                    email = "helmut@goerke.de"
                    phone = "0911"
                    mobilePhone = "0151"
                  }
              )
        actual |> should equal expected
        (Server.getUserSession().GetLoggedInUser() |> Async.RunSynchronously).IsSome |> should equal true

    [<Fact>]
    member this.``login with existing email and incorrect password should result in Failure``() =
        let actual = loginWithEmailAndPassword' "rene.ederer.nbg@gmail.com" "wrongPassword" (DB.GetDataContext())
        let expected : Result<string * User> = Failure "Email or password is wrong."

        Server.getUserSession().GetLoggedInUser() |> Async.RunSynchronously |> should equal None
        actual |> should equal expected

    [<Fact>]
    member this.``login with nonexisting email should result in Failure``() =
        let expected : Result<string * User> = Failure "Email or password is wrong."
        let actual = loginWithEmailAndPassword' "nonexisting@email.com" "somePassword" this.dbContext
        Server.getUserSession().GetLoggedInUser() |> Async.RunSynchronously |> should equal None
        actual |> should equal expected

    [<Fact>]
    member this.``register with already registered email should result in Failure``() =
        let actual = register' "rene.ederer.nbg@gmail.com" "password" this.dbContext
        let expected : Result<string * User> = Failure "This email is already registered"
        actual |> should equal expected
        Server.getUserSession().GetLoggedInUser() |> Async.RunSynchronously |> should equal None

    [<Fact>]
    member this.``logout should set loggedInUser to None and the sessionGuid to None``() =
        Server.getUserSession().LoginUser("1") |> Async.RunSynchronously
        let actual = logout' 1 this.dbContext 
        let expected : Result<unit> = Ok ()
        actual |> should equal expected
        Server.getUserSession().GetLoggedInUser() |> Async.RunSynchronously |> should equal None

