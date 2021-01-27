module Wizard.Projects.Routes exposing (Route(..))

import Shared.Data.PaginationQueryString exposing (PaginationQueryString)
import Uuid exposing (Uuid)
import Wizard.Projects.Detail.ProjectDetailRoute exposing (ProjectDetailRoute)


type Route
    = CreateRoute (Maybe String)
    | CreateMigrationRoute Uuid
    | DetailRoute Uuid ProjectDetailRoute
    | IndexRoute PaginationQueryString
    | MigrationRoute Uuid
