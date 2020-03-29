module Wizard.Users.Create.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Users.Common.UserCreateForm as UserCreateForm exposing (UserCreateForm)


type alias Model =
    { savingUser : ActionResult String
    , form : Form CustomFormError UserCreateForm
    }


initialModel : AppState -> Model
initialModel appState =
    { savingUser = Unset
    , form = UserCreateForm.init appState
    }
