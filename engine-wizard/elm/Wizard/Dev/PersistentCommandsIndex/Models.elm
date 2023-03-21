module Wizard.Dev.PersistentCommandsIndex.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Shared.Data.PaginationQueryFilters as PaginationQueryFilters
import Shared.Data.PaginationQueryString exposing (PaginationQueryString)
import Shared.Data.PersistentCommand exposing (PersistentCommand)
import Wizard.Common.Components.Listing.Models as Listing
import Wizard.Dev.Routes exposing (persistentCommandIndexRouteStateFilterId)


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
