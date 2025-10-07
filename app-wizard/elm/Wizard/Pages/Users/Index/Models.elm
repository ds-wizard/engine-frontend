module Wizard.Pages.Users.Index.Models exposing (Model, initialModel)

import ActionResult exposing (ActionResult(..))
import Common.Data.PaginationQueryFilters as PaginationQueryFilters
import Common.Data.PaginationQueryString exposing (PaginationQueryString)
import Wizard.Api.Models.User exposing (User)
import Wizard.Components.Listing.Models as Listing
import Wizard.Pages.Users.Routes exposing (indexRouteRoleFilterId)


type alias Model =
    { users : Listing.Model User
    , userToBeDeleted : Maybe User
    , deletingUser : ActionResult String
    }


initialModel : PaginationQueryString -> Maybe String -> Model
initialModel paginationQueryString mbRoute =
    let
        paginationQueryFilters =
            PaginationQueryFilters.fromValues [ ( indexRouteRoleFilterId, mbRoute ) ]
    in
    { users = Listing.initialModelWithFilters paginationQueryString paginationQueryFilters
    , userToBeDeleted = Nothing
    , deletingUser = Unset
    }
