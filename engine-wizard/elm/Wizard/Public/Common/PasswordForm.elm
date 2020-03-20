module Wizard.Public.Common.PasswordForm exposing (PasswordForm, encode, initEmpty, validation)

import Form exposing (Form)
import Form.Validate as V exposing (Validation)
import Json.Encode as E exposing (..)
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Common.Form.Validate as V


type alias PasswordForm =
    { password : String
    , passwordConfirmation : String
    }


initEmpty : Form CustomFormError PasswordForm
initEmpty =
    Form.initial [] validation


validation : Validation CustomFormError PasswordForm
validation =
    V.map2 PasswordForm
        (V.field "password" V.string)
        (V.field "password" V.string |> V.confirmation "passwordConfirmation")


encode : PasswordForm -> E.Value
encode form =
    E.object
        [ ( "password", E.string form.password )
        ]
