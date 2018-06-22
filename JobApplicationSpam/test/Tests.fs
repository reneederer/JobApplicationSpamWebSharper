namespace JobApplicationSpam

open FsUnit.Xunit
open Server.Internal
open Types
open System.Transactions

open System
open System.Linq
open Xunit

module Server = Server.Internal


type InitTest () =
    let options : FSharp.Data.Sql.Transactions.TransactionOptions =
            { IsolationLevel = FSharp.Data.Sql.Transactions.IsolationLevel.ReadCommitted
              Timeout = TimeSpan.FromSeconds 100000. }
    member val scope =
        new TransactionScope(TransactionScopeOption.RequiresNew, TransactionOptions(IsolationLevel = System.Transactions.IsolationLevel.ReadCommitted)) with get, set
    interface IDisposable with
        member this.Dispose() =
            Server.getUserSession().Logout() |> Async.RunSynchronously
            this.scope.Dispose()
            this.scope <-
                new TransactionScope(TransactionScopeOption.RequiresNew, TransactionOptions(IsolationLevel = IsolationLevel.ReadCommitted))

type MyTests ()  =
    inherit InitTest()
    let dbContext () =
        let options : FSharp.Data.Sql.Transactions.TransactionOptions =
                { IsolationLevel = FSharp.Data.Sql.Transactions.IsolationLevel.ReadCommitted
                  Timeout = TimeSpan.FromSeconds 100000. }
        DB.GetDataContext("Server=localhost; Port=5432; User Id=spam; Password=Steinmetzstr9!@#$; Database=jobapplicationspamtest; Enlist=true", options, 100000)

    [<Fact>]
    member this.``login with correct email and password and confirmed email should set the loggedInUser to (Some userId) and return a LoggedInUser``() =
        let actual =
            match loginWithEmailAndPassword' "rene.ederer.nbg@gmail.com" "1234" (dbContext()) with
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
        let context = dbContext()
        let actualReturnedSessionGuid, actualReturnedUser =
            match register' "nonexisting@email.com" "password" context with
            | Ok (sessionGuid, user) -> sessionGuid, user
            | _ -> failwith "failure"

        let actualDbEmail, actualDbSessionGuid, actualDbPassword, actualDbSalt, actualDbUserId =
            (query {
                for dbUserValues in context.Public.Uservalues do
                join dbUser in context.Public.Users on (dbUserValues.Userid = dbUser.Id)
                where (dbUserValues.Email = "nonexisting@email.com")
                select (dbUserValues.Email, dbUser.Sessionguid, dbUser.Password, dbUser.Salt, dbUser.Id)
            }).Single()

        let expectedUser =
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

        ( Some actualReturnedSessionGuid,
          actualReturnedUser,
          actualDbEmail,
          actualDbPassword,
          Some (string actualDbUserId))
        |> should equal
            ( actualDbSessionGuid,
              expectedUser,
              "nonexisting@email.com",
              Server.generateHashWithSalt "password" actualDbSalt 1000 64,
              Server.getUserSession().GetLoggedInUser() |> Async.RunSynchronously
            )

    [<Fact>]
    member this.``login with correct email and password and unconfirmed email should set the loggedInUser to (Some userId) and return a Guest``() =
        let actual =
            match loginWithEmailAndPassword' "helmut@goerke.de" "1" (dbContext()) with
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
        let actual = loginWithEmailAndPassword' "rene.ederer.nbg@gmail.com" "wrongPassword" (dbContext())
        let expected : Result<string * User> = Failure "Email or password is wrong."

        Server.getUserSession().GetLoggedInUser() |> Async.RunSynchronously |> should equal None
        actual |> should equal expected

    [<Fact>]
    member this.``login with nonexisting email should result in Failure``() =
        let expected : Result<string * User> = Failure "Email or password is wrong."
        let actual = loginWithEmailAndPassword' "nonexisting@email.com" "somePassword" (dbContext())
        Server.getUserSession().GetLoggedInUser() |> Async.RunSynchronously |> should equal None
        actual |> should equal expected

    [<Fact>]
    member this.``register with already registered email should result in Failure``() =
        let actual = register' "rene.ederer.nbg@gmail.com" "password" (dbContext())
        let expected : Result<string * User> = Failure "This email is already registered"
        actual |> should equal expected
        Server.getUserSession().GetLoggedInUser() |> Async.RunSynchronously |> should equal None

    [<Fact>]
    member this.``logout should set loggedInUser to None and the sessionGuid to None``() =
        Server.getUserSession().LoginUser("1") |> Async.RunSynchronously
        let actual = logout' 1 (dbContext())
        let expected : Result<unit> = Ok ()
        actual |> should equal expected
        Server.getUserSession().GetLoggedInUser() |> Async.RunSynchronously |> should equal None


























