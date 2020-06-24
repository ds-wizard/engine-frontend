module Wizard.Public.ForgottenPassword.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Shared.Form.FormError exposing (FormError)
import Wizard.Public.Common.ForgottenPasswordForm as ForgottenPasswordForm exposing (ForgottenPasswordForm)


type alias Model =
    { form : Form FormError ForgottenPasswordForm
    , submitting : ActionResult String
    }


initialModel : Model
initialModel =
    { form = ForgottenPasswordForm.initEmpty
    , submitting = Unset
    }
