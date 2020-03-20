module Wizard.Users.Common.UserPasswordForm exposing (UserPasswordForm, encode, init, validation)

import Form exposing (Form)
import Form.Validate as V exposing (Validation)
import Json.Encode as E
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Common.Form.Validate as V


type alias UserPasswordForm =
    { password : String
    , passwordConfirmation : String
    }


init : Form CustomFormError UserPasswordForm
init =
    Form.initial [] validation


validation : Validation CustomFormError UserPasswordForm
validation =
    V.map2 UserPasswordForm
        (V.field "password" V.string)
        (V.field "password" V.string |> V.confirmation "passwordConfirmation")


encode : UserPasswordForm -> E.Value
encode form =
    E.object
        [ ( "password", E.string form.password )
        ]
