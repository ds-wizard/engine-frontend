module Wizard.Public.Signup.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Shared.Form.FormError exposing (FormError)
import Wizard.Public.Common.SignupForm as SignupForm exposing (SignupForm)


type alias Model =
    { form : Form FormError SignupForm
    , signingUp : ActionResult String
    }


initialModel : Model
initialModel =
    { form = SignupForm.initEmpty
    , signingUp = Unset
    }
