module Public.ForgottenPasswordConfirmation.Models exposing (..)

import ActionResult exposing (ActionResult(..))
import Common.Form exposing (CustomFormError)
import Common.Form.Validate exposing (validateConfirmation)
import Form exposing (Form)
import Form.Validate as Validate exposing (Validation)
import Json.Encode as Encode exposing (..)


type alias Model =
    { form : Form CustomFormError PasswordForm
    , submitting : ActionResult String
    , userId : String
    , hash : String
    }


initialModel : String -> String -> Model
initialModel userId hash =
    { form = initEmptyPasswordForm
    , submitting = Unset
    , userId = userId
    , hash = hash
    }


type alias PasswordForm =
    { password : String
    , passwordConfirmation : String
    }


initEmptyPasswordForm : Form CustomFormError PasswordForm
initEmptyPasswordForm =
    Form.initial [] passwordFormValidation


passwordFormValidation : Validation CustomFormError PasswordForm
passwordFormValidation =
    Validate.map2 PasswordForm
        (Validate.field "password" Validate.string)
        (Validate.field "password" Validate.string |> validateConfirmation "passwordConfirmation")


encodePasswordForm : PasswordForm -> Encode.Value
encodePasswordForm form =
    Encode.object
        [ ( "password", Encode.string form.password )
        ]
