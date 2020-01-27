module Wizard.Users.Index.Models exposing (Model, initialModel)

import ActionResult exposing (ActionResult(..))
import Wizard.Common.Components.Listing as Listing
import Wizard.Users.Common.User exposing (User)


type alias Model =
    { users : ActionResult (Listing.Model User)
    , userToBeDeleted : Maybe User
    , deletingUser : ActionResult String
    }


initialModel : Model
initialModel =
    { users = Loading
    , userToBeDeleted = Nothing
    , deletingUser = Unset
    }
