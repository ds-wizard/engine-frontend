module UserManagement.Create.Models exposing (..)

import Common.Types exposing (ActionResult(..))
import Form exposing (Form)
import UserManagement.Models exposing (UserCreateForm, initUserCreateForm)


type alias Model =
    { savingUser : ActionResult String
    , form : Form () UserCreateForm
    }


initialModel : Model
initialModel =
    { savingUser = Unset
    , form = initUserCreateForm
    }
