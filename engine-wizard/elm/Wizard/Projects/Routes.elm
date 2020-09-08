module Wizard.Projects.Routes exposing (Route(..))

import Shared.Data.PaginationQueryString exposing (PaginationQueryString)
import Uuid exposing (Uuid)
import Wizard.Projects.Detail.PlanDetailRoute exposing (PlanDetailRoute)


type Route
    = CreateRoute (Maybe String)
    | CreateMigrationRoute Uuid
    | DetailRoute Uuid PlanDetailRoute
    | IndexRoute PaginationQueryString
    | MigrationRoute Uuid
