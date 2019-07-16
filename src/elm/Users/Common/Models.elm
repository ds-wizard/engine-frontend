module Users.Common.Models exposing (User, roles, userDecoder, userListDecoder)

import Json.Decode as Decode exposing (..)
import Json.Decode.Extra as Decode
import Json.Decode.Pipeline exposing (required)


type alias User =
    { uuid : String
    , email : String
    , name : String
    , surname : String
    , role : String
    , active : Bool
    }


userDecoder : Decoder User
userDecoder =
    Decode.succeed User
        |> required "uuid" Decode.string
        |> required "email" Decode.string
        |> required "name" Decode.string
        |> required "surname" Decode.string
        |> required "role" Decode.string
        |> required "active" Decode.bool


userListDecoder : Decoder (List User)
userListDecoder =
    Decode.list userDecoder


roles : List String
roles =
    [ "ADMIN", "DATASTEWARD", "RESEARCHER" ]
