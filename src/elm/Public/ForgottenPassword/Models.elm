module Public.ForgottenPassword.Models exposing (..)

import Common.Form exposing (CustomFormError)
import Common.Types exposing (ActionResult(..))
import Form exposing (Form)
import Form.Validate as Validate exposing (..)
import Json.Encode as Encode exposing (..)


type alias Model =
    { form : Form CustomFormError ForgottenPasswordForm
    , submitting : ActionResult String
    }


initialModel : Model
initialModel =
    { form = initEmptyForgottenPasswordForm
    , submitting = Unset
    }


type alias ForgottenPasswordForm =
    { email : String
    }


initEmptyForgottenPasswordForm : Form CustomFormError ForgottenPasswordForm
initEmptyForgottenPasswordForm =
    Form.initial [] forgottenPasswordFormValidation


forgottenPasswordFormValidation : Validation CustomFormError ForgottenPasswordForm
forgottenPasswordFormValidation =
    Validate.map ForgottenPasswordForm
        (Validate.field "email" Validate.email)


encodeForgottenPasswordForm : ForgottenPasswordForm -> Encode.Value
encodeForgottenPasswordForm form =
    Encode.object
        [ ( "type", Encode.string "password" )
        , ( "email", Encode.string form.email )
        ]
