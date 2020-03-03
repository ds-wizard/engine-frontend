module Wizard.Users.Common.UserCreateForm exposing (UserCreateForm, encode, init, validation)

import Form exposing (Form)
import Form.Validate as Validate exposing (..)
import Json.Encode as E exposing (..)
import Wizard.Common.Form exposing (CustomFormError)


type alias UserCreateForm =
    { email : String
    , firstName : String
    , lastName : String
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
        (Validate.field "firstName" Validate.string)
        (Validate.field "lastName" Validate.string)
        (Validate.field "role" Validate.string)
        (Validate.field "password" Validate.string)


encode : String -> UserCreateForm -> E.Value
encode uuid form =
    E.object
        [ ( "uuid", E.string uuid )
        , ( "email", E.string form.email )
        , ( "firstName", E.string form.firstName )
        , ( "lastName", E.string form.lastName )
        , ( "role", E.string form.role )
        , ( "password", E.string form.password )
        ]
