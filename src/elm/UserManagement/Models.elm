module UserManagement.Models exposing (..)

import Common.Form.Validate exposing (CustomFormError, validateConfirmation)
import Form exposing (Form)
import Form.Field as Field
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
    [ "ADMIN", "DATASTEWARD", "RESEARCHER" ]


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
    Encode.object
        [ ( "uuid", Encode.string uuid )
        , ( "email", Encode.string form.email )
        , ( "name", Encode.string form.name )
        , ( "surname", Encode.string form.surname )
        , ( "role", Encode.string form.role )
        , ( "password", Encode.string form.password )
        ]


type alias UserEditForm =
    { email : String
    , name : String
    , surname : String
    , role : String
    }


initEmptyUserEditForm : Form () UserEditForm
initEmptyUserEditForm =
    Form.initial [] userEditFormValidation


initUserEditForm : User -> Form () UserEditForm
initUserEditForm user =
    Form.initial (userToUserEditFormInitials user) userEditFormValidation


userEditFormValidation : Validation () UserEditForm
userEditFormValidation =
    Validate.map4 UserEditForm
        (Validate.field "email" Validate.email)
        (Validate.field "name" Validate.string)
        (Validate.field "surname" Validate.string)
        (Validate.field "role" Validate.string)


encodeUserEditForm : String -> UserEditForm -> Encode.Value
encodeUserEditForm uuid form =
    Encode.object
        [ ( "uuid", Encode.string uuid )
        , ( "email", Encode.string form.email )
        , ( "name", Encode.string form.name )
        , ( "surname", Encode.string form.surname )
        , ( "role", Encode.string form.role )
        ]


userToUserEditFormInitials : User -> List ( String, Field.Field )
userToUserEditFormInitials user =
    [ ( "email", Field.string user.email )
    , ( "name", Field.string user.name )
    , ( "surname", Field.string user.surname )
    , ( "role", Field.string user.role )
    ]


type alias UserPasswordForm =
    { password : String
    , passwordConfirmation : String
    }


initUserPasswordForm : Form CustomFormError UserPasswordForm
initUserPasswordForm =
    Form.initial [] userPasswordFormValidation


userPasswordFormValidation : Validation CustomFormError UserPasswordForm
userPasswordFormValidation =
    Validate.map2 UserPasswordForm
        (Validate.field "password" Validate.string)
        (Validate.field "password" Validate.string |> validateConfirmation "passwordConfirmation")


encodeUserPasswordForm : UserPasswordForm -> Encode.Value
encodeUserPasswordForm form =
    Encode.object
        [ ( "password", Encode.string form.password )
        ]
