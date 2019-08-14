module Public.Signup.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Common.Form exposing (CustomFormError)
import Form exposing (Form)
import Public.Common.SignupForm as SignupForm exposing (SignupForm)


type alias Model =
    { form : Form CustomFormError SignupForm
    , signingUp : ActionResult String
    }


initialModel : Model
initialModel =
    { form = SignupForm.initEmpty
    , signingUp = Unset
    }
