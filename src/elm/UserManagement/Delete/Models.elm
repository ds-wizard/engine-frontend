module UserManagement.Delete.Models exposing (..)

import Common.Types exposing (ActionResult(..))
import UserManagement.Models exposing (User)


type alias Model =
    { user : ActionResult User
    , deletingUser : ActionResult String
    }


initialModel : Model
initialModel =
    { user = Loading
    , deletingUser = Unset
    }
