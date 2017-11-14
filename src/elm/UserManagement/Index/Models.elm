module UserManagement.Index.Models exposing (..)

import Common.Types exposing (ActionResult(..))
import UserManagement.Models exposing (User)


type alias Model =
    { users : ActionResult (List User)
    }


initialModel : Model
initialModel =
    { users = Loading
    }
