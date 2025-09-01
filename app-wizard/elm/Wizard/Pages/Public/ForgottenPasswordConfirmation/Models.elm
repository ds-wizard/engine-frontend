module Wizard.Pages.Public.ForgottenPasswordConfirmation.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Shared.Utils.Form.FormError exposing (FormError)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Public.Common.PasswordForm as PasswordForm exposing (PasswordForm)


type alias Model =
    { form : Form FormError PasswordForm
    , submitting : ActionResult String
    , userId : String
    , hash : String
    }


initialModel : AppState -> String -> String -> Model
initialModel appState userId hash =
    { form = PasswordForm.initEmpty appState
    , submitting = Unset
    , userId = userId
    , hash = hash
    }
