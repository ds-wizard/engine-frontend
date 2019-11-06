module Wizard.Public.ForgottenPassword.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Public.Common.ForgottenPasswordForm as ForgottenPasswordForm exposing (ForgottenPasswordForm)


type alias Model =
    { form : Form CustomFormError ForgottenPasswordForm
    , submitting : ActionResult String
    }


initialModel : Model
initialModel =
    { form = ForgottenPasswordForm.initEmpty
    , submitting = Unset
    }
