module Wizard.Users.Common.User exposing (User, compare, decoder, isAdmin, roles)

import Json.Decode as D exposing (..)
import Json.Decode.Pipeline as D


type alias User =
    { uuid : String
    , email : String
    , name : String
    , surname : String
    , role : String
    , active : Bool
    }


decoder : Decoder User
decoder =
    D.succeed User
        |> D.required "uuid" D.string
        |> D.required "email" D.string
        |> D.required "name" D.string
        |> D.required "surname" D.string
        |> D.required "role" D.string
        |> D.required "active" D.bool


roles : List String
roles =
    [ "ADMIN", "DATASTEWARD", "RESEARCHER" ]


isAdmin : Maybe User -> Bool
isAdmin =
    Maybe.map (.role >> (==) "ADMIN") >> Maybe.withDefault False


compare : User -> User -> Order
compare u1 u2 =
    case Basics.compare (String.toLower u1.surname) (String.toLower u2.surname) of
        LT ->
            LT

        GT ->
            GT

        EQ ->
            Basics.compare (String.toLower u1.name) (String.toLower u2.name)
