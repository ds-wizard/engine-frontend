module UserManagement.Common.Models exposing (..)

import Common.Form.Validate exposing (CustomFormError, validateConfirmation)
import Form exposing (Form)
import Form.Field as Field
import Form.Validate as Validate exposing (..)
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (decode, required)


type alias User =
    { uuid : String
    , email : String
    , name : String
    , surname : String
    , role : String
    }


userDecoder : Decoder User
userDecoder =
    decode User
        |> required "uuid" Decode.string
        |> required "email" Decode.string
        |> required "name" Decode.string
        |> required "surname" Decode.string
        |> required "role" Decode.string


userListDecoder : Decoder (List User)
userListDecoder =
    Decode.list userDecoder


roles : List String
roles =
    [ "ADMIN", "DATASTEWARD", "RESEARCHER" ]
