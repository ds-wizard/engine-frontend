module Wizard.Pages.Users.Create.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult)
import Common.Api.Models.Role exposing (Role)
import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Users.Common.UserCreateForm as UserCreateForm exposing (UserCreateForm)


type alias Model =
    { roles : ActionResult (List Role)
    , savingUser : ActionResult String
    , form : Form FormError UserCreateForm
    }


initialModel : AppState -> Model
initialModel appState =
    { roles = ActionResult.Loading
    , savingUser = ActionResult.Unset
    , form = UserCreateForm.init appState
    }
