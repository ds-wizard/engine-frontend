module Wizard.Pages.Dev.Routes exposing (Route(..), persistentCommandIndexRouteStateFilterId)

import Common.Data.PaginationQueryString exposing (PaginationQueryString)
import Uuid exposing (Uuid)


type Route
    = OperationsRoute
    | PersistentCommandsDetail Uuid
    | PersistentCommandsIndex PaginationQueryString (Maybe String)


persistentCommandIndexRouteStateFilterId : String
persistentCommandIndexRouteStateFilterId =
    "state"
