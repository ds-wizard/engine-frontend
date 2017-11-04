module UserManagement.Models exposing (..)

import Form exposing (Form)
import Form.Validate as Validate exposing (..)
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as Encode exposing (..)


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


type alias UserCreateForm =
    { email : String
    , name : String
    , surname : String
    , role : String
    , password : String
    }


roles : List String
roles =
    [ "ADMIN", "DATA_STEWARD", "RESEARCHER" ]


initUserCreateForm : Form () UserCreateForm
initUserCreateForm =
    Form.initial [] userCreateFormValidation


userCreateFormValidation : Validation () UserCreateForm
userCreateFormValidation =
    Validate.map5 UserCreateForm
        (Validate.field "email" Validate.email)
        (Validate.field "name" Validate.string)
        (Validate.field "surname" Validate.string)
        (Validate.field "role" Validate.string)
        (Validate.field "password" Validate.string)


encodeUserCreateForm : String -> UserCreateForm -> Encode.Value
encodeUserCreateForm uuid form =
    let
        fields =
            [ ( "uuid", Encode.string uuid )
            , ( "email", Encode.string form.email )
            , ( "name", Encode.string form.name )
            , ( "surname", Encode.string form.surname )
            , ( "role", Encode.string form.role )
            , ( "password", Encode.string form.password )
            ]
    in
    Encode.object fields
