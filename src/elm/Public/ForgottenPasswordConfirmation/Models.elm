module Public.ForgottenPasswordConfirmation.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Common.Form exposing (CustomFormError)
import Form exposing (Form)
import Public.Common.PasswordForm as PasswordForm exposing (PasswordForm)


type alias Model =
    { form : Form CustomFormError PasswordForm
    , submitting : ActionResult String
    , userId : String
    , hash : String
    }


initialModel : String -> String -> Model
initialModel userId hash =
    { form = PasswordForm.initEmpty
    , submitting = Unset
    , userId = userId
    , hash = hash
    }
