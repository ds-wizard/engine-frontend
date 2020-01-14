module Wizard.Public.Signup.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Public.Common.SignupForm as SignupForm exposing (SignupForm)


type alias Model =
    { form : Form CustomFormError SignupForm
    , signingUp : ActionResult String
    }


initialModel : Model
initialModel =
    { form = SignupForm.initEmpty
    , signingUp = Unset
    }
