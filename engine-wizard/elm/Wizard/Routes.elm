module Wizard.Routes exposing
    ( Route(..)
    , appHome
    , appsCreate
    , appsDetail
    , appsIndex
    , appsIndexWithFilters
    , devOperations
    , documentTemplateEditorCreate
    , documentTemplateEditorDetail
    , documentTemplateEditorsIndex
    , documentTemplateEditorsIndexWithFilters
    , documentTemplatesDetail
    , documentTemplatesImport
    , documentTemplatesIndex
    , documentTemplatesIndexWithFilters
    , documentsIndex
    , documentsIndexWithFilters
    , isAppIndex
    , isDevOperations
    , isDevSubroute
    , isDocumentTemplateEditorsIndex
    , isDocumentTemplatesIndex
    , isDocumentTemplatesSubroute
    , isDocumentsIndex
    , isKmEditorEditor
    , isKmEditorIndex
    , isKnowledgeModelsIndex
    , isKnowledgeModelsSubroute
    , isLocalesRoute
    , isPersistentCommandsIndex
    , isProjectImportersIndex
    , isProjectSubroute
    , isProjectsDetail
    , isProjectsIndex
    , isSettingsRoute
    , isSettingsSubroute
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
    , localesCreate
    , localesDetail
    , localesImport
    , localesIndex
    , persistentCommandsDetail
    , persistentCommandsIndex
    , persistentCommandsIndexWithFilters
    , projectImport
    , projectImportersIndex
    , projectsCreate
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
    , settingsAuthentication
    , settingsDefault
    , settingsLookAndFeel
    , settingsOrganization
    , settingsRegistry
    , usersCreate
    , usersEdit
    , usersEditCurrent
    , usersIndex
    , usersIndexWithFilters
    )

import Shared.Auth.Session as Session exposing (Session)
import Shared.Data.BootstrapConfig exposing (BootstrapConfig)
import Shared.Data.PaginationQueryFilters as PaginationQueryFilters exposing (PaginationQueryFilters)
import Shared.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Shared.Data.Questionnaire.QuestionnaireCreation as QuestionnaireCreation
import Uuid exposing (Uuid)
import Wizard.Apps.Routes
import Wizard.Dev.Routes
import Wizard.DocumentTemplateEditors.Routes
import Wizard.DocumentTemplates.Routes
import Wizard.Documents.Routes
import Wizard.KMEditor.Editor.KMEditorRoute
import Wizard.KMEditor.Routes
import Wizard.KnowledgeModels.Routes
import Wizard.Locales.Routes
import Wizard.ProjectImporters.Routes
import Wizard.Projects.Create.ProjectCreateRoute
import Wizard.Projects.Detail.ProjectDetailRoute
import Wizard.Projects.Routes
import Wizard.Public.Routes
import Wizard.Registry.Routes
import Wizard.Settings.Routes
import Wizard.Users.Routes


type Route
    = AppsRoute Wizard.Apps.Routes.Route
    | DashboardRoute
    | DevRoute Wizard.Dev.Routes.Route
    | DocumentsRoute Wizard.Documents.Routes.Route
    | DocumentTemplateEditorsRoute Wizard.DocumentTemplateEditors.Routes.Route
    | DocumentTemplatesRoute Wizard.DocumentTemplates.Routes.Route
    | KMEditorRoute Wizard.KMEditor.Routes.Route
    | KnowledgeModelsRoute Wizard.KnowledgeModels.Routes.Route
    | LocalesRoute Wizard.Locales.Routes.Route
    | ProjectsRoute Wizard.Projects.Routes.Route
    | ProjectImportersRoute Wizard.ProjectImporters.Routes.Route
    | PublicRoute Wizard.Public.Routes.Route
    | RegistryRoute Wizard.Registry.Routes.Route
    | SettingsRoute Wizard.Settings.Routes.Route
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


isDevOperations : Route -> Bool
isDevOperations =
    (==) (DevRoute Wizard.Dev.Routes.OperationsRoute)


persistentCommandsIndex : Route
persistentCommandsIndex =
    DevRoute (Wizard.Dev.Routes.PersistentCommandsIndex PaginationQueryString.empty Nothing)


isPersistentCommandsIndex : Route -> Bool
isPersistentCommandsIndex route =
    case route of
        DevRoute (Wizard.Dev.Routes.PersistentCommandsIndex _ _) ->
            True

        _ ->
            False


persistentCommandsIndexWithFilters : PaginationQueryFilters -> PaginationQueryString -> Route
persistentCommandsIndexWithFilters filters pagination =
    DevRoute
        (Wizard.Dev.Routes.PersistentCommandsIndex pagination
            (PaginationQueryFilters.getValue Wizard.Dev.Routes.persistentCommandIndexRouteStateFilterId filters)
        )


persistentCommandsDetail : Uuid -> Route
persistentCommandsDetail =
    DevRoute << Wizard.Dev.Routes.PersistentCommandsDetail


isDevSubroute : Route -> Bool
isDevSubroute route =
    case route of
        DevRoute _ ->
            True

        _ ->
            False



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



-- Document Templates


documentTemplatesDetail : String -> Route
documentTemplatesDetail =
    DocumentTemplatesRoute << Wizard.DocumentTemplates.Routes.DetailRoute


documentTemplatesImport : Maybe String -> Route
documentTemplatesImport =
    DocumentTemplatesRoute << Wizard.DocumentTemplates.Routes.ImportRoute


documentTemplatesIndex : Route
documentTemplatesIndex =
    DocumentTemplatesRoute (Wizard.DocumentTemplates.Routes.IndexRoute PaginationQueryString.empty)


documentTemplatesIndexWithFilters : PaginationQueryFilters -> PaginationQueryString -> Route
documentTemplatesIndexWithFilters _ pagination =
    DocumentTemplatesRoute (Wizard.DocumentTemplates.Routes.IndexRoute pagination)


isDocumentTemplatesIndex : Route -> Bool
isDocumentTemplatesIndex route =
    case route of
        DocumentTemplatesRoute (Wizard.DocumentTemplates.Routes.IndexRoute _) ->
            True

        _ ->
            False


isDocumentTemplatesSubroute : Route -> Bool
isDocumentTemplatesSubroute route =
    case route of
        DocumentTemplatesRoute _ ->
            True

        DocumentTemplateEditorsRoute _ ->
            True

        _ ->
            False



-- Document Template Editors


documentTemplateEditorCreate : Maybe String -> Maybe Bool -> Route
documentTemplateEditorCreate mbBasedOn mbEdit =
    DocumentTemplateEditorsRoute <| Wizard.DocumentTemplateEditors.Routes.CreateRoute mbBasedOn mbEdit


documentTemplateEditorDetail : String -> Route
documentTemplateEditorDetail =
    DocumentTemplateEditorsRoute << Wizard.DocumentTemplateEditors.Routes.EditorRoute


documentTemplateEditorsIndex : Route
documentTemplateEditorsIndex =
    DocumentTemplateEditorsRoute (Wizard.DocumentTemplateEditors.Routes.IndexRoute PaginationQueryString.empty)


documentTemplateEditorsIndexWithFilters : PaginationQueryFilters -> PaginationQueryString -> Route
documentTemplateEditorsIndexWithFilters _ pagination =
    DocumentTemplateEditorsRoute (Wizard.DocumentTemplateEditors.Routes.IndexRoute pagination)


isDocumentTemplateEditorsIndex : Route -> Bool
isDocumentTemplateEditorsIndex route =
    case route of
        DocumentTemplateEditorsRoute (Wizard.DocumentTemplateEditors.Routes.IndexRoute _) ->
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


isKmEditorEditor : Uuid -> Route -> Bool
isKmEditorEditor uuid route =
    case route of
        KMEditorRoute (Wizard.KMEditor.Routes.EditorRoute editorUuid _) ->
            uuid == editorUuid

        _ ->
            False


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


isKnowledgeModelsSubroute : Route -> Bool
isKnowledgeModelsSubroute route =
    case route of
        KnowledgeModelsRoute _ ->
            True

        KMEditorRoute _ ->
            True

        _ ->
            False



-- Project Importers


projectImportersIndex : Route
projectImportersIndex =
    ProjectImportersRoute (Wizard.ProjectImporters.Routes.IndexRoute PaginationQueryString.empty)


isProjectImportersIndex : Route -> Bool
isProjectImportersIndex route =
    case route of
        ProjectImportersRoute (Wizard.ProjectImporters.Routes.IndexRoute _) ->
            True

        _ ->
            False



-- Projects


projectsCreate : { a | config : BootstrapConfig } -> Route
projectsCreate appState =
    if QuestionnaireCreation.fromTemplateEnabled appState.config.questionnaire.questionnaireCreation then
        projectsCreateTemplate Nothing

    else
        projectsCreateCustom Nothing


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


projectsDetailDocumentsNew : Uuid -> Maybe Uuid -> Route
projectsDetailDocumentsNew uuid mbEventUuid =
    ProjectsRoute <| Wizard.Projects.Routes.DetailRoute uuid <| Wizard.Projects.Detail.ProjectDetailRoute.NewDocument mbEventUuid


projectsDetailSettings : Uuid -> Route
projectsDetailSettings uuid =
    ProjectsRoute <| Wizard.Projects.Routes.DetailRoute uuid <| Wizard.Projects.Detail.ProjectDetailRoute.Settings


isProjectsDetail : Uuid -> Route -> Bool
isProjectsDetail uuid route =
    case route of
        ProjectsRoute (Wizard.Projects.Routes.DetailRoute projectUuid _) ->
            uuid == projectUuid

        _ ->
            False


projectsIndex : { a | session : Session } -> Route
projectsIndex appState =
    let
        mbUserUuid =
            Session.getUserUuid appState.session
    in
    ProjectsRoute (Wizard.Projects.Routes.IndexRoute PaginationQueryString.empty Nothing mbUserUuid Nothing Nothing Nothing Nothing Nothing)


projectsIndexWithFilters : PaginationQueryFilters -> PaginationQueryString -> Route
projectsIndexWithFilters filters pagination =
    ProjectsRoute
        (Wizard.Projects.Routes.IndexRoute pagination
            (PaginationQueryFilters.getValue Wizard.Projects.Routes.indexRouteIsTemplateFilterId filters)
            (PaginationQueryFilters.getValue Wizard.Projects.Routes.indexRouteUsersFilterId filters)
            (PaginationQueryFilters.getOp Wizard.Projects.Routes.indexRouteUsersFilterId filters)
            (PaginationQueryFilters.getValue Wizard.Projects.Routes.indexRouteProjectTagsFilterId filters)
            (PaginationQueryFilters.getOp Wizard.Projects.Routes.indexRouteProjectTagsFilterId filters)
            (PaginationQueryFilters.getValue Wizard.Projects.Routes.indexRoutePackagesFilterId filters)
            (PaginationQueryFilters.getOp Wizard.Projects.Routes.indexRoutePackagesFilterId filters)
        )


isProjectsIndex : Route -> Bool
isProjectsIndex route =
    case route of
        ProjectsRoute (Wizard.Projects.Routes.IndexRoute _ _ _ _ _ _ _ _) ->
            True

        _ ->
            False


projectsMigration : Uuid -> Route
projectsMigration =
    ProjectsRoute << Wizard.Projects.Routes.MigrationRoute


projectImport : Uuid -> String -> Route
projectImport uuid importerId =
    ProjectsRoute <| Wizard.Projects.Routes.ImportRoute uuid importerId


isProjectSubroute : Route -> Bool
isProjectSubroute route =
    case route of
        ProjectsRoute _ ->
            True

        ProjectImportersRoute _ ->
            True

        _ ->
            False



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


settingsDefault : Route
settingsDefault =
    SettingsRoute Wizard.Settings.Routes.defaultRoute


isSettingsRoute : Route -> Bool
isSettingsRoute route =
    case route of
        SettingsRoute _ ->
            True

        _ ->
            False


isSettingsSubroute : Route -> Bool
isSettingsSubroute route =
    case route of
        SettingsRoute _ ->
            True

        UsersRoute _ ->
            True

        LocalesRoute _ ->
            True

        _ ->
            False


settingsAuthentication : Route
settingsAuthentication =
    SettingsRoute Wizard.Settings.Routes.AuthenticationRoute


settingsLookAndFeel : Route
settingsLookAndFeel =
    SettingsRoute Wizard.Settings.Routes.LookAndFeelRoute


settingsOrganization : Route
settingsOrganization =
    SettingsRoute Wizard.Settings.Routes.OrganizationRoute


settingsRegistry : Route
settingsRegistry =
    SettingsRoute Wizard.Settings.Routes.RegistryRoute



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



-- Locales


isLocalesRoute : Route -> Bool
isLocalesRoute route =
    case route of
        LocalesRoute (Wizard.Locales.Routes.IndexRoute _) ->
            True

        _ ->
            False


localesCreate : Route
localesCreate =
    LocalesRoute <| Wizard.Locales.Routes.CreateRoute


localesImport : Maybe String -> Route
localesImport =
    LocalesRoute << Wizard.Locales.Routes.ImportRoute


localesIndex : Route
localesIndex =
    LocalesRoute (Wizard.Locales.Routes.IndexRoute PaginationQueryString.empty)


localesDetail : String -> Route
localesDetail =
    LocalesRoute << Wizard.Locales.Routes.DetailRoute
