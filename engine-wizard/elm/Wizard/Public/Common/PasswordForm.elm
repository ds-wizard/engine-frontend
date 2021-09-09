module Wizard.Public.Common.PasswordForm exposing
    ( PasswordForm
    , encode
    , initEmpty
    , validation
    )

import Form exposing (Form)
import Form.Validate as V exposing (Validation)
import Json.Encode as E
import Shared.Form.FormError exposing (FormError)
import Shared.Form.Validate as V


type alias PasswordForm =
    { password : String
    , passwordConfirmation : String
    }


initEmpty : Form FormError PasswordForm
initEmpty =
    Form.initial [] validation


validation : Validation FormError PasswordForm
validation =
    V.map2 PasswordForm
        (V.field "password" V.string)
        (V.field "password" V.string |> V.confirmation "passwordConfirmation")


encode : PasswordForm -> E.Value
encode form =
    E.object
        [ ( "password", E.string form.password )
        ]
