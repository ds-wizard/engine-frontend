module Wizard.Public.Common.PasswordForm exposing (PasswordForm, encode, initEmpty, validation)

import Form exposing (Form)
import Form.Validate as Validate exposing (Validation)
import Json.Encode as E exposing (..)
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Common.Form.Validate exposing (validateConfirmation)


type alias PasswordForm =
    { password : String
    , passwordConfirmation : String
    }


initEmpty : Form CustomFormError PasswordForm
initEmpty =
    Form.initial [] validation


validation : Validation CustomFormError PasswordForm
validation =
    Validate.map2 PasswordForm
        (Validate.field "password" Validate.string)
        (Validate.field "password" Validate.string |> validateConfirmation "passwordConfirmation")


encode : PasswordForm -> E.Value
encode form =
    E.object
        [ ( "password", E.string form.password )
        ]
