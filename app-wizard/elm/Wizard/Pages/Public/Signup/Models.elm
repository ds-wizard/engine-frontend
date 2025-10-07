module Wizard.Pages.Public.Signup.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Public.Common.SignupForm as SignupForm exposing (SignupForm)


type alias Model =
    { form : Form FormError SignupForm
    , signingUp : ActionResult String
    }


initialModel : AppState -> Model
initialModel appState =
    { form = SignupForm.initEmpty appState
    , signingUp = Unset
    }
