module Wizard.Pages.Public.Common.ForgottenPasswordForm exposing
    ( ForgottenPasswordForm
    , encode
    , initEmpty
    , validation
    )

import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Form.Validate as Validate exposing (Validation)
import Json.Encode as E


type alias ForgottenPasswordForm =
    { email : String
    }


initEmpty : Form FormError ForgottenPasswordForm
initEmpty =
    Form.initial [] validation


validation : Validation FormError ForgottenPasswordForm
validation =
    Validate.map ForgottenPasswordForm
        (Validate.field "email" Validate.email)


encode : ForgottenPasswordForm -> E.Value
encode form =
    E.object
        [ ( "type", E.string "ForgottenPasswordActionKey" )
        , ( "email", E.string form.email )
        ]
