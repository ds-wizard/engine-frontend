module Wizard.Users.Common.User exposing
    ( User
    , compare
    , decoder
    , fullName
    , isAdmin
    )

import Json.Decode as D exposing (..)
import Json.Decode.Pipeline as D
import Wizard.Users.Common.Role as Role


type alias User =
    { uuid : String
    , email : String
    , firstName : String
    , lastName : String
    , affiliation : Maybe String
    , role : String
    , active : Bool
    , sources : List String
    }


decoder : Decoder User
decoder =
    D.succeed User
        |> D.required "uuid" D.string
        |> D.required "email" D.string
        |> D.required "firstName" D.string
        |> D.required "lastName" D.string
        |> D.required "affiliation" (D.maybe D.string)
        |> D.required "role" D.string
        |> D.required "active" D.bool
        |> D.required "sources" (D.list D.string)


isAdmin : Maybe User -> Bool
isAdmin =
    Maybe.map (.role >> (==) Role.admin) >> Maybe.withDefault False


compare : User -> User -> Order
compare u1 u2 =
    case Basics.compare (String.toLower u1.lastName) (String.toLower u2.lastName) of
        LT ->
            LT

        GT ->
            GT

        EQ ->
            Basics.compare (String.toLower u1.firstName) (String.toLower u2.firstName)


fullName : User -> String
fullName user =
    user.firstName ++ " " ++ user.lastName
