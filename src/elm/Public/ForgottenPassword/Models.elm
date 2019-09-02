module Public.ForgottenPassword.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Common.Form exposing (CustomFormError)
import Form exposing (Form)
import Public.Common.ForgottenPasswordForm as ForgottenPasswordForm exposing (ForgottenPasswordForm)


type alias Model =
    { form : Form CustomFormError ForgottenPasswordForm
    , submitting : ActionResult String
    }


initialModel : Model
initialModel =
    { form = ForgottenPasswordForm.initEmpty
    , submitting = Unset
    }
