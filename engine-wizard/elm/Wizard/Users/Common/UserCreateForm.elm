module Wizard.Users.Common.UserCreateForm exposing (UserCreateForm, encode, init, validation)

import Form exposing (Form)
import Form.Validate as Validate exposing (..)
import Json.Encode as E exposing (..)
import Wizard.Common.Form exposing (CustomFormError)


type alias UserCreateForm =
    { email : String
    , name : String
    , surname : String
    , role : String
    , password : String
    }


init : Form CustomFormError UserCreateForm
init =
    Form.initial [] validation


validation : Validation CustomFormError UserCreateForm
validation =
    Validate.map5 UserCreateForm
        (Validate.field "email" Validate.email)
        (Validate.field "name" Validate.string)
        (Validate.field "surname" Validate.string)
        (Validate.field "role" Validate.string)
        (Validate.field "password" Validate.string)


encode : String -> UserCreateForm -> E.Value
encode uuid form =
    E.object
        [ ( "uuid", E.string uuid )
        , ( "email", E.string form.email )
        , ( "name", E.string form.name )
        , ( "surname", E.string form.surname )
        , ( "role", E.string form.role )
        , ( "password", E.string form.password )
        ]
