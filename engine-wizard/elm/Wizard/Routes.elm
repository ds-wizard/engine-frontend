module Wizard.Routes exposing
    ( Route(..)
    , appsDetail
    , appsIndex
    , appsIndexWithFilters
    , documentsIndex
    , documentsIndexWithFilters
    , isAppIndex
    , isDocumentsIndex
    , isKmEditorIndex
    , isKnowledgeModelsIndex
    , isProjectsIndex
    , isTemplateIndex
    , isUsersIndex
    , kmEditorEditor
    , kmEditorIndex
    , kmEditorIndexWithFilters
    , kmEditorMigration
    , knowledgeModelsIndex
    , knowledgeModelsIndexWithFilters
    , projectIndexWithFilters
    , projectsIndex
    , templatesIndex
    , templatesIndexWithFilters
    , usersIndex
    , usersIndexWithFilters
    )

import Shared.Data.PaginationQueryFilters as PaginationQueryFilters exposing (PaginationQueryFilters)
import Shared.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Uuid exposing (Uuid)
import Wizard.Admin.Routes
import Wizard.Apps.Routes
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
    | AppsRoute Wizard.Apps.Routes.Route
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


appsIndex : Route
appsIndex =
    AppsRoute (Wizard.Apps.Routes.IndexRoute PaginationQueryString.empty Nothing)


appsIndexWithFilters : PaginationQueryFilters -> PaginationQueryString -> Route
appsIndexWithFilters filters pagination =
    AppsRoute
        (Wizard.Apps.Routes.IndexRoute pagination
            (PaginationQueryFilters.getValue Wizard.Apps.Routes.indexRouteEnabledFilterId filters)
        )


appsDetail : Uuid -> Route
appsDetail =
    AppsRoute << Wizard.Apps.Routes.DetailRoute


isAppIndex : Route -> Bool
isAppIndex route =
    case route of
        AppsRoute (Wizard.Apps.Routes.IndexRoute _ _) ->
            True

        _ ->
            False


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


documentsIndexWithFilters : PaginationQueryFilters -> PaginationQueryString -> Route
documentsIndexWithFilters _ pagination =
    DocumentsRoute (Wizard.Documents.Routes.IndexRoute Nothing pagination)


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


kmEditorIndexWithFilters : PaginationQueryFilters -> PaginationQueryString -> Route
kmEditorIndexWithFilters _ pagination =
    KMEditorRoute (Wizard.KMEditor.Routes.IndexRoute pagination)


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


kmEditorMigration : Uuid -> Route
kmEditorMigration branchUUid =
    KMEditorRoute (Wizard.KMEditor.Routes.MigrationRoute branchUUid)


knowledgeModelsIndex : Route
knowledgeModelsIndex =
    KnowledgeModelsRoute (Wizard.KnowledgeModels.Routes.IndexRoute PaginationQueryString.empty)


knowledgeModelsIndexWithFilters : PaginationQueryFilters -> PaginationQueryString -> Route
knowledgeModelsIndexWithFilters _ pagination =
    KnowledgeModelsRoute (Wizard.KnowledgeModels.Routes.IndexRoute pagination)


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


templatesIndexWithFilters : PaginationQueryFilters -> PaginationQueryString -> Route
templatesIndexWithFilters _ pagination =
    TemplatesRoute (Wizard.Templates.Routes.IndexRoute pagination)


isTemplateIndex : Route -> Bool
isTemplateIndex route =
    case route of
        TemplatesRoute (Wizard.Templates.Routes.IndexRoute _) ->
            True

        _ ->
            False
