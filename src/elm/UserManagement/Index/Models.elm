module UserManagement.Index.Models exposing (..)

{-|

@docs Model, initialModel

-}

import Common.Types exposing (ActionResult(..))
import UserManagement.Models exposing (User)


{-| -}
type alias Model =
    { users : ActionResult (List User)
    , userToBeDeleted : Maybe User
    , deletingUser : ActionResult String
    }


{-| -}
initialModel : Model
initialModel =
    { users = Loading
    , userToBeDeleted = Nothing
    , deletingUser = Unset
    }
