module Wizard.Projects.Routes exposing
    ( Route(..)
    , indexRouteIsTemplateFilterId
    , indexRouteProjectTagsFilterId
    , indexRouteUsersFilterId
    )

import Shared.Data.PaginationQueryFilters.FilterOperator exposing (FilterOperator)
import Shared.Data.PaginationQueryString exposing (PaginationQueryString)
import Uuid exposing (Uuid)
import Wizard.Projects.Create.ProjectCreateRoute exposing (ProjectCreateRoute)
import Wizard.Projects.Detail.ProjectDetailRoute exposing (ProjectDetailRoute)


type Route
    = CreateRoute ProjectCreateRoute
    | CreateMigrationRoute Uuid
    | DetailRoute Uuid ProjectDetailRoute
    | IndexRoute PaginationQueryString (Maybe String) (Maybe String) (Maybe FilterOperator) (Maybe String) (Maybe FilterOperator)
    | MigrationRoute Uuid
    | ImportRoute Uuid String


indexRouteUsersFilterId : String
indexRouteUsersFilterId =
    "userUuids"


indexRouteIsTemplateFilterId : String
indexRouteIsTemplateFilterId =
    "isTemplate"


indexRouteProjectTagsFilterId : String
indexRouteProjectTagsFilterId =
    "projectTags"
