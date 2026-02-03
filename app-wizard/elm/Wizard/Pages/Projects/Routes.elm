module Wizard.Pages.Projects.Routes exposing
    ( Route(..)
    , indexRouteIsTemplateFilterId
    , indexRouteKnowledgeModelPackagesFilterId
    , indexRouteProjectTagsFilterId
    , indexRouteUsersFilterId
    )

import Common.Data.PaginationQueryFilters.FilterOperator exposing (FilterOperator)
import Common.Data.PaginationQueryString exposing (PaginationQueryString)
import Uuid exposing (Uuid)
import Wizard.Pages.Projects.Detail.ProjectDetailRoute exposing (ProjectDetailRoute)


type Route
    = CreateRoute (Maybe Uuid) (Maybe String)
    | CreateMigrationRoute Uuid
    | DetailRoute Uuid ProjectDetailRoute
    | IndexRoute PaginationQueryString (Maybe String) (Maybe String) (Maybe FilterOperator) (Maybe String) (Maybe FilterOperator) (Maybe String) (Maybe FilterOperator)
    | MigrationRoute Uuid
    | ImportRoute Uuid String
    | ImportLegacyRoute Uuid String
    | DocumentDownloadRoute Uuid Uuid
    | FileDownloadRoute Uuid Uuid


indexRouteUsersFilterId : String
indexRouteUsersFilterId =
    "userUuids"


indexRouteIsTemplateFilterId : String
indexRouteIsTemplateFilterId =
    "isTemplate"


indexRouteProjectTagsFilterId : String
indexRouteProjectTagsFilterId =
    "projectTags"


indexRouteKnowledgeModelPackagesFilterId : String
indexRouteKnowledgeModelPackagesFilterId =
    "knowledgeModelPackages"
