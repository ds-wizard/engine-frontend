module Wizard.Pages.Projects.Routes exposing
    ( Route(..)
    , indexRouteIsTemplateFilterId
    , indexRouteKnowledgeModelPackagesFilterId
    , indexRouteProjectTagsFilterId
    , indexRouteUserGroupsFilterId
    , indexRouteUsersFilterId
    )

import Common.Data.PaginationQueryFilters.FilterOperator exposing (FilterOperator)
import Common.Data.PaginationQueryString exposing (PaginationQueryString)
import Uuid exposing (Uuid)
import Wizard.Pages.Projects.Detail.ProjectDetailRoute exposing (ProjectDetailRoute)


type Route
    = CreateRoute (Maybe Uuid) (Maybe Uuid)
    | CreateMigrationRoute Uuid
    | DetailRoute Uuid ProjectDetailRoute
    | IndexRoute PaginationQueryString (Maybe String) (Maybe String) (Maybe FilterOperator) (Maybe String) (Maybe FilterOperator) (Maybe String) (Maybe FilterOperator) (Maybe String) (Maybe FilterOperator)
    | ImportRoute Uuid String
    | DocumentDownloadRoute Uuid Uuid
    | FileDownloadRoute Uuid Uuid


indexRouteUsersFilterId : String
indexRouteUsersFilterId =
    "userUuids"


indexRouteUserGroupsFilterId : String
indexRouteUserGroupsFilterId =
    "userGroupUuids"


indexRouteIsTemplateFilterId : String
indexRouteIsTemplateFilterId =
    "isTemplate"


indexRouteProjectTagsFilterId : String
indexRouteProjectTagsFilterId =
    "projectTags"


indexRouteKnowledgeModelPackagesFilterId : String
indexRouteKnowledgeModelPackagesFilterId =
    "knowledgeModelPackages"
