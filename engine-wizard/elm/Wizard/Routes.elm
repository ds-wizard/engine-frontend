module Wizard.Routes exposing
    ( Route(..)
    , appHome
    , appsCreate
    , appsDetail
    , appsIndex
    , appsIndexWithFilters
    , devOperations
    , documentsIndex
    , documentsIndexWithFilters
    , isAppIndex
    , isDocumentsIndex
    , isKmEditorIndex
    , isKnowledgeModelsIndex
    , isProjectsIndex
    , isTemplateIndex
    , isUsersIndex
    , kmEditorCreate
    , kmEditorEditor
    , kmEditorEditorPreview
    , kmEditorEditorQuestionTags
    , kmEditorEditorSettings
    , kmEditorIndex
    , kmEditorIndexWithFilters
    , kmEditorMigration
    , kmEditorPublish
    , knowledgeModelsDetail
    , knowledgeModelsImport
    , knowledgeModelsIndex
    , knowledgeModelsIndexWithFilters
    , knowledgeModelsPreview
    , persistentCommandsDetail
    , persistentCommandsIndex
    , persistentCommandsIndexWithFilters
    , projectsCreateCustom
    , projectsCreateMigration
    , projectsCreateTemplate
    , projectsDetailDocuments
    , projectsDetailDocumentsNew
    , projectsDetailQuestionnaire
    , projectsDetailSettings
    , projectsIndex
    , projectsIndexWithFilters
    , projectsMigration
    , publicForgottenPassword
    , publicHome
    , publicLogin
    , publicSignup
    , settingsRegistry
    , templatesDetail
    , templatesImport
    , templatesIndex
    , templatesIndexWithFilters
    , usersCreate
    , usersEdit
    , usersEditCurrent
    , usersIndex
    , usersIndexWithFilters
    )

import Shared.Data.PaginationQueryFilters as PaginationQueryFilters exposing (PaginationQueryFilters)
import Shared.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Uuid exposing (Uuid)
import Wizard.Apps.Routes
import Wizard.Dev.Routes
import Wizard.Documents.Routes
import Wizard.KMEditor.Editor.KMEditorRoute
import Wizard.KMEditor.Routes
import Wizard.KnowledgeModels.Routes
import Wizard.Projects.Create.ProjectCreateRoute
import Wizard.Projects.Detail.ProjectDetailRoute
import Wizard.Projects.Routes
import Wizard.Public.Routes
import Wizard.Registry.Routes
import Wizard.Settings.Routes
import Wizard.Templates.Routes
import Wizard.Users.Routes


type Route
    = AppsRoute Wizard.Apps.Routes.Route
    | DashboardRoute
    | DevRoute Wizard.Dev.Routes.Route
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


publicHome : Route
publicHome =
    PublicRoute <| Wizard.Public.Routes.LoginRoute Nothing


appHome : Route
appHome =
    DashboardRoute



-- Apps


appsCreate : Route
appsCreate =
    AppsRoute Wizard.Apps.Routes.CreateRoute


appsDetail : Uuid -> Route
appsDetail =
    AppsRoute << Wizard.Apps.Routes.DetailRoute


appsIndex : Route
appsIndex =
    AppsRoute (Wizard.Apps.Routes.IndexRoute PaginationQueryString.empty Nothing)


appsIndexWithFilters : PaginationQueryFilters -> PaginationQueryString -> Route
appsIndexWithFilters filters pagination =
    AppsRoute
        (Wizard.Apps.Routes.IndexRoute pagination
            (PaginationQueryFilters.getValue Wizard.Apps.Routes.indexRouteEnabledFilterId filters)
        )


isAppIndex : Route -> Bool
isAppIndex route =
    case route of
        AppsRoute (Wizard.Apps.Routes.IndexRoute _ _) ->
            True

        _ ->
            False



-- Dev


devOperations : Route
devOperations =
    DevRoute Wizard.Dev.Routes.OperationsRoute


persistentCommandsIndex : Route
persistentCommandsIndex =
    DevRoute (Wizard.Dev.Routes.PersistentCommandsIndex PaginationQueryString.empty Nothing)


persistentCommandsIndexWithFilters : PaginationQueryFilters -> PaginationQueryString -> Route
persistentCommandsIndexWithFilters filters pagination =
    DevRoute
        (Wizard.Dev.Routes.PersistentCommandsIndex pagination
            (PaginationQueryFilters.getValue Wizard.Dev.Routes.persistentCommandIndexRouteStateFilterId filters)
        )


persistentCommandsDetail : Uuid -> Route
persistentCommandsDetail =
    DevRoute << Wizard.Dev.Routes.PersistentCommandsDetail



-- Documents


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



-- KM Editor


kmEditorCreate : Maybe String -> Maybe Bool -> Route
kmEditorCreate mbKmId mbEdit =
    KMEditorRoute <| Wizard.KMEditor.Routes.CreateRoute mbKmId mbEdit


kmEditorEditor : Uuid -> Maybe Uuid -> Route
kmEditorEditor branchUuid mbEntityUuid =
    KMEditorRoute (Wizard.KMEditor.Routes.EditorRoute branchUuid (Wizard.KMEditor.Editor.KMEditorRoute.Edit mbEntityUuid))


kmEditorEditorQuestionTags : Uuid -> Route
kmEditorEditorQuestionTags branchUuid =
    KMEditorRoute (Wizard.KMEditor.Routes.EditorRoute branchUuid Wizard.KMEditor.Editor.KMEditorRoute.QuestionTags)


kmEditorEditorPreview : Uuid -> Route
kmEditorEditorPreview branchUuid =
    KMEditorRoute (Wizard.KMEditor.Routes.EditorRoute branchUuid Wizard.KMEditor.Editor.KMEditorRoute.Preview)


kmEditorEditorSettings : Uuid -> Route
kmEditorEditorSettings branchUuid =
    KMEditorRoute (Wizard.KMEditor.Routes.EditorRoute branchUuid Wizard.KMEditor.Editor.KMEditorRoute.Settings)


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


kmEditorMigration : Uuid -> Route
kmEditorMigration =
    KMEditorRoute << Wizard.KMEditor.Routes.MigrationRoute


kmEditorPublish : Uuid -> Route
kmEditorPublish =
    KMEditorRoute << Wizard.KMEditor.Routes.PublishRoute



-- Knowledge Models


knowledgeModelsDetail : String -> Route
knowledgeModelsDetail =
    KnowledgeModelsRoute << Wizard.KnowledgeModels.Routes.DetailRoute


knowledgeModelsImport : Maybe String -> Route
knowledgeModelsImport =
    KnowledgeModelsRoute << Wizard.KnowledgeModels.Routes.ImportRoute


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


knowledgeModelsPreview : String -> Maybe String -> Route
knowledgeModelsPreview packageId mbQuestionUuid =
    KnowledgeModelsRoute <| Wizard.KnowledgeModels.Routes.PreviewRoute packageId mbQuestionUuid



-- Projects


projectsCreateCustom : Maybe String -> Route
projectsCreateCustom =
    ProjectsRoute << Wizard.Projects.Routes.CreateRoute << Wizard.Projects.Create.ProjectCreateRoute.CustomCreateRoute


projectsCreateTemplate : Maybe String -> Route
projectsCreateTemplate =
    ProjectsRoute << Wizard.Projects.Routes.CreateRoute << Wizard.Projects.Create.ProjectCreateRoute.TemplateCreateRoute


projectsCreateMigration : Uuid -> Route
projectsCreateMigration =
    ProjectsRoute << Wizard.Projects.Routes.CreateMigrationRoute


projectsDetailQuestionnaire : Uuid -> Route
projectsDetailQuestionnaire uuid =
    ProjectsRoute <| Wizard.Projects.Routes.DetailRoute uuid Wizard.Projects.Detail.ProjectDetailRoute.Questionnaire


projectsDetailDocuments : Uuid -> Route
projectsDetailDocuments uuid =
    ProjectsRoute <| Wizard.Projects.Routes.DetailRoute uuid <| Wizard.Projects.Detail.ProjectDetailRoute.Documents PaginationQueryString.empty


projectsDetailDocumentsNew : Uuid -> Maybe String -> Route
projectsDetailDocumentsNew uuid mbEventUuidString =
    ProjectsRoute <| Wizard.Projects.Routes.DetailRoute uuid <| Wizard.Projects.Detail.ProjectDetailRoute.NewDocument mbEventUuidString


projectsDetailSettings : Uuid -> Route
projectsDetailSettings uuid =
    ProjectsRoute <| Wizard.Projects.Routes.DetailRoute uuid <| Wizard.Projects.Detail.ProjectDetailRoute.Settings


projectsIndex : Route
projectsIndex =
    ProjectsRoute (Wizard.Projects.Routes.IndexRoute PaginationQueryString.empty Nothing Nothing Nothing Nothing Nothing)


projectsIndexWithFilters : PaginationQueryFilters -> PaginationQueryString -> Route
projectsIndexWithFilters filters pagination =
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


projectsMigration : Uuid -> Route
projectsMigration =
    ProjectsRoute << Wizard.Projects.Routes.MigrationRoute



-- Public


publicForgottenPassword : Route
publicForgottenPassword =
    PublicRoute Wizard.Public.Routes.ForgottenPasswordRoute


publicLogin : Maybe String -> Route
publicLogin originalUrl =
    PublicRoute <| Wizard.Public.Routes.LoginRoute originalUrl


publicSignup : Route
publicSignup =
    PublicRoute Wizard.Public.Routes.SignupRoute



-- Settings


settingsRegistry : Route
settingsRegistry =
    SettingsRoute Wizard.Settings.Routes.RegistryRoute



-- Templates


templatesDetail : String -> Route
templatesDetail =
    TemplatesRoute << Wizard.Templates.Routes.DetailRoute


templatesImport : Maybe String -> Route
templatesImport =
    TemplatesRoute << Wizard.Templates.Routes.ImportRoute


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



-- Users


usersCreate : Route
usersCreate =
    UsersRoute Wizard.Users.Routes.CreateRoute


usersEdit : String -> Route
usersEdit =
    UsersRoute << Wizard.Users.Routes.EditRoute


usersEditCurrent : Route
usersEditCurrent =
    usersEdit "current"


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
