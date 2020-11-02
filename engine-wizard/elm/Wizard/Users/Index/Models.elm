module Wizard.Users.Index.Models exposing (Model, initialModel)

import ActionResult exposing (ActionResult(..))
import Shared.Data.PaginationQueryString exposing (PaginationQueryString)
import Shared.Data.User exposing (User)
import Wizard.Common.Components.Listing.Models as Listing


type alias Model =
    { users : Listing.Model User
    , userToBeDeleted : Maybe User
    , deletingUser : ActionResult String
    }


initialModel : PaginationQueryString -> Model
initialModel paginationQueryString =
    { users = Listing.initialModel paginationQueryString
    , userToBeDeleted = Nothing
    , deletingUser = Unset
    }
