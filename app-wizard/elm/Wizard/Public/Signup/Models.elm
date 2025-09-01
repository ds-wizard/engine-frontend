module Wizard.Public.Signup.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Shared.Utils.Form.FormError exposing (FormError)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Public.Common.SignupForm as SignupForm exposing (SignupForm)


type alias Model =
    { form : Form FormError SignupForm
    , signingUp : ActionResult String
    }


initialModel : AppState -> Model
initialModel appState =
    { form = SignupForm.initEmpty appState
    , signingUp = Unset
    }
