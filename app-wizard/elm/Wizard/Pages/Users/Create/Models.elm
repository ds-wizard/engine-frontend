module Wizard.Pages.Users.Create.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Users.Common.UserCreateForm as UserCreateForm exposing (UserCreateForm)


type alias Model =
    { savingUser : ActionResult String
    , form : Form FormError UserCreateForm
    }


initialModel : AppState -> Model
initialModel appState =
    { savingUser = Unset
    , form = UserCreateForm.init appState
    }
