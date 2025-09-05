module Wizard.Pages.Dev.PersistentCommandsIndex.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Common.Data.PaginationQueryFilters as PaginationQueryFilters
import Common.Data.PaginationQueryString exposing (PaginationQueryString)
import Wizard.Api.Models.PersistentCommand exposing (PersistentCommand)
import Wizard.Components.Listing.Models as Listing
import Wizard.Pages.Dev.Routes exposing (persistentCommandIndexRouteStateFilterId)


type alias Model =
    { persistentCommands : Listing.Model PersistentCommand
    , updating : ActionResult String
    }


initialModel : PaginationQueryString -> Maybe String -> Model
initialModel paginationQueryString mbState =
    let
        paginationQueryFilters =
            PaginationQueryFilters.create
                [ ( persistentCommandIndexRouteStateFilterId, mbState ) ]
                []
    in
    { persistentCommands = Listing.initialModelWithFilters paginationQueryString paginationQueryFilters
    , updating = Unset
    }
