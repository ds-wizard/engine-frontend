module Wizard.Users.Create.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Shared.Utils.Form.FormError exposing (FormError)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Users.Common.UserCreateForm as UserCreateForm exposing (UserCreateForm)


type alias Model =
    { savingUser : ActionResult String
    , form : Form FormError UserCreateForm
    }


initialModel : AppState -> Model
initialModel appState =
    { savingUser = Unset
    , form = UserCreateForm.init appState
    }
