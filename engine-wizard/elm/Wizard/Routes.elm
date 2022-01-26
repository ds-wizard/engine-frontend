module Wizard.Routes exposing
    ( Route(..)
    , documentsIndex
    , isDocumentsIndex
    , isKmEditorIndex
    , isKnowledgeModelsIndex
    , isProjectsIndex
    , isTemplateIndex
    , isUsersIndex
    , kmEditorEditor
    , kmEditorIndex
    , knowledgeModelsIndex
    , projectIndexWithFilters
    , projectsIndex
    , templatesIndex
    , usersIndex
    , usersIndexWithFilters
    )

import Shared.Data.PaginationQueryFilters as PaginationQueryFilters exposing (PaginationQueryFilters)
import Shared.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Uuid exposing (Uuid)
import Wizard.Admin.Routes
import Wizard.Documents.Routes
import Wizard.KMEditor.Editor.KMEditorRoute
import Wizard.KMEditor.Routes
import Wizard.KnowledgeModels.Routes
import Wizard.Projects.Routes
import Wizard.Public.Routes
import Wizard.Registry.Routes
import Wizard.Settings.Routes
import Wizard.Templates.Routes
import Wizard.Users.Routes


type Route
    = AdminRoute Wizard.Admin.Routes.Route
    | DashboardRoute
    | DocumentsRoute Wizard.Documents.Routes.Route
    | KMEditorRoute Wizard.KMEditor.Routes.Route
    | KnowledgeModelsRoute Wizard.KnowledgeModels.Routes.Route
    | ProjectsRoute Wizard.Projects.Routes.Route
    | PublicRoute Wizard.Public.Routes.Route
    | RegistryRoute Wizard.Registry.Routes.Route
    | SettingsRoute Wizard.Settings.Routes.Route
    | TemplatesRoute Wizard.Templates.Routes.Route
    | UsersRoute Wizard.Users.Routes.Route
    | NotAllowedRoute
    | NotFoundRoute


usersIndex : Route
usersIndex =
    UsersRoute (Wizard.Users.Routes.IndexRoute PaginationQueryString.empty Nothing)


usersIndexWithFilters : PaginationQueryFilters -> PaginationQueryString -> Route
usersIndexWithFilters filters pagination =
    UsersRoute
        (Wizard.Users.Routes.IndexRoute pagination
            (PaginationQueryFilters.getValue Wizard.Users.Routes.indexRouteRoleFilterId filters)
        )


isUsersIndex : Route -> Bool
isUsersIndex route =
    case route of
        UsersRoute (Wizard.Users.Routes.IndexRoute _ _) ->
            True

        _ ->
            False


projectsIndex : Route
projectsIndex =
    ProjectsRoute (Wizard.Projects.Routes.IndexRoute PaginationQueryString.empty Nothing Nothing Nothing Nothing Nothing)


projectIndexWithFilters : PaginationQueryFilters -> PaginationQueryString -> Route
projectIndexWithFilters filters pagination =
    ProjectsRoute
        (Wizard.Projects.Routes.IndexRoute pagination
            (PaginationQueryFilters.getValue Wizard.Projects.Routes.indexRouteIsTemplateFilterId filters)
            (PaginationQueryFilters.getValue Wizard.Projects.Routes.indexRouteUsersFilterId filters)
            (PaginationQueryFilters.getOp Wizard.Projects.Routes.indexRouteUsersFilterId filters)
            (PaginationQueryFilters.getValue Wizard.Projects.Routes.indexRouteProjectTagsFilterId filters)
            (PaginationQueryFilters.getOp Wizard.Projects.Routes.indexRouteProjectTagsFilterId filters)
        )


isProjectsIndex : Route -> Bool
isProjectsIndex route =
    case route of
        ProjectsRoute (Wizard.Projects.Routes.IndexRoute _ _ _ _ _ _) ->
            True

        _ ->
            False


documentsIndex : Route
documentsIndex =
    DocumentsRoute (Wizard.Documents.Routes.IndexRoute Nothing PaginationQueryString.empty)


isDocumentsIndex : Route -> Bool
isDocumentsIndex route =
    case route of
        DocumentsRoute (Wizard.Documents.Routes.IndexRoute _ _) ->
            True

        _ ->
            False


kmEditorIndex : Route
kmEditorIndex =
    KMEditorRoute (Wizard.KMEditor.Routes.IndexRoute PaginationQueryString.empty)


isKmEditorIndex : Route -> Bool
isKmEditorIndex route =
    case route of
        KMEditorRoute (Wizard.KMEditor.Routes.IndexRoute _) ->
            True

        _ ->
            False


kmEditorEditor : Uuid -> Maybe Uuid -> Route
kmEditorEditor branchUuid mbEntityUuid =
    KMEditorRoute (Wizard.KMEditor.Routes.EditorRoute branchUuid (Wizard.KMEditor.Editor.KMEditorRoute.Edit mbEntityUuid))


knowledgeModelsIndex : Route
knowledgeModelsIndex =
    KnowledgeModelsRoute (Wizard.KnowledgeModels.Routes.IndexRoute PaginationQueryString.empty)


isKnowledgeModelsIndex : Route -> Bool
isKnowledgeModelsIndex route =
    case route of
        KnowledgeModelsRoute (Wizard.KnowledgeModels.Routes.IndexRoute _) ->
            True

        _ ->
            False


templatesIndex : Route
templatesIndex =
    TemplatesRoute (Wizard.Templates.Routes.IndexRoute PaginationQueryString.empty)


isTemplateIndex : Route -> Bool
isTemplateIndex route =
    case route of
        TemplatesRoute (Wizard.Templates.Routes.IndexRoute _) ->
            True

        _ ->
            False
