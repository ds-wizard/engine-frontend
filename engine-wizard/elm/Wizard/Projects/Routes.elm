module Wizard.Projects.Routes exposing
    ( Route(..)
    , indexRouteIsTemplateFilterId
    , indexRouteUsersFilterId
    )

import Shared.Data.PaginationQueryString exposing (PaginationQueryString)
import Uuid exposing (Uuid)
import Wizard.Projects.Create.ProjectCreateRoute exposing (ProjectCreateRoute)
import Wizard.Projects.Detail.ProjectDetailRoute exposing (ProjectDetailRoute)


type Route
    = CreateRoute ProjectCreateRoute
    | CreateMigrationRoute Uuid
    | DetailRoute Uuid ProjectDetailRoute
    | IndexRoute PaginationQueryString (Maybe String) (Maybe String)
    | MigrationRoute Uuid


indexRouteUsersFilterId : String
indexRouteUsersFilterId =
    "userUuids"


indexRouteIsTemplateFilterId : String
indexRouteIsTemplateFilterId =
    "isTemplate"
