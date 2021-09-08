module Wizard.Users.Index.Models exposing (Model, initialModel)

import ActionResult exposing (ActionResult(..))
import Shared.Data.PaginationQueryString exposing (PaginationQueryString)
import Shared.Data.User exposing (User)
import Shared.Utils exposing (dictFromMaybeList)
import Wizard.Common.Components.Listing.Models as Listing
import Wizard.Users.Routes exposing (indexRouteRoleFilterId)


type alias Model =
    { users : Listing.Model User
    , userToBeDeleted : Maybe User
    , deletingUser : ActionResult String
    }


initialModel : PaginationQueryString -> Maybe String -> Model
initialModel paginationQueryString mbRoute =
    let
        filters =
            dictFromMaybeList [ ( indexRouteRoleFilterId, mbRoute ) ]
    in
    { users = Listing.initialModelWithFilters paginationQueryString filters
    , userToBeDeleted = Nothing
    , deletingUser = Unset
    }
