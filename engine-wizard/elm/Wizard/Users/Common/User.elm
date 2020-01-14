module Wizard.Users.Common.User exposing (User, decoder, isAdmin, roles)

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
