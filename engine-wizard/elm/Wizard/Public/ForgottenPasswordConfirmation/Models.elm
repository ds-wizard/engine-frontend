module Wizard.Public.ForgottenPasswordConfirmation.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Public.Common.PasswordForm as PasswordForm exposing (PasswordForm)


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
