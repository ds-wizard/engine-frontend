module Wizard.Routes exposing
    ( Route(..)
    , appHome
    , commentsIndex
    , commentsIndexWithFilters
    , commentsRouteResolvedFilterId
    , dashboard
    , devOperations
    , documentTemplateEditorCreate
    , documentTemplateEditorDetail
    , documentTemplateEditorDetailFiles
    , documentTemplateEditorDetailPreview
    , documentTemplateEditorDetailSettings
    , documentTemplateEditorsIndex
    , documentTemplateEditorsIndexWithFilters
    , documentTemplatesDetail
    , documentTemplatesImport
    , documentTemplatesIndex
    , documentTemplatesIndexWithFilters
    , documentsIndex
    , documentsIndexWithFilters
    , isDashboard
    , isDevOperations
    , isDevSubroute
    , isDocumentTemplateEditor
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
    , isProjectActionsIndex
    , isProjectFilesIndex
    , isProjectImportersIndex
    , isProjectSubroute
    , isProjectsDetail
    , isProjectsIndex
    , isSameListingRoute
    , isSettingsRoute
    , isSettingsSubroute
    , isTenantIndex
    , isUsersIndex
    , kmEditorCreate
    , kmEditorEditor
    , kmEditorEditorPhases
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
    , knowledgeModelsResourcePage
    , localesCreate
    , localesDetail
    , localesImport
    , localesIndex
    , localesIndexWithFilters
    , persistentCommandsDetail
    , persistentCommandsIndex
    , persistentCommandsIndexWithFilters
    , projectActionsIndex
    , projectActionsIndexWithFilters
    , projectDocumentDownload
    , projectFilesIndex
    , projectFilesIndexWithFilters
    , projectImportersIndex
    , projectImportersIndexWithFilters
    , projectsCreate
    , projectsCreateFromKnowledgeModel
    , projectsCreateFromProjectTemplate
    , projectsCreateMigration
    , projectsDetail
    , projectsDetailDocuments
    , projectsDetailDocumentsNew
    , projectsDetailDocumentsWithFilters
    , projectsDetailFilesWithFilters
    , projectsDetailQuestionnaire
    , projectsDetailSettings
    , projectsFileDownload
    , projectsImport
    , projectsIndex
    , projectsIndexWithFilters
    , projectsMigration
    , publicForgottenPassword
    , publicHome
    , publicLogin
    , publicLogoutSuccessful
    , publicSignup
    , settingsAuthentication
    , settingsDefault
    , settingsLookAndFeel
    , settingsOrganization
    , settingsRegistry
    , tenantsCreate
    , tenantsDetail
    , tenantsIndex
    , tenantsIndexWithFilters
    , usersCreate
    , usersEdit
    , usersEditActiveSessions
    , usersEditApiKeys
    , usersEditAppKeys
    , usersEditCurrent
    , usersEditLanguage
    , usersEditLanguageCurrent
    , usersEditPassword
    , usersEditSubmissionSettings
    , usersEditTours
    , usersIndex
    , usersIndexWithFilters
    )

import Shared.Auth.Role as Role
import Shared.Auth.Session exposing (Session)
import Shared.Common.UuidOrCurrent as UuidOrCurrent exposing (UuidOrCurrent)
import Shared.Data.PaginationQueryFilters as PaginationQueryFilters exposing (PaginationQueryFilters)
import Shared.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Shared.Utils exposing (flip)
import Uuid exposing (Uuid)
import Wizard.Api.Models.BootstrapConfig exposing (BootstrapConfig)
import Wizard.Dev.Routes
import Wizard.DocumentTemplateEditors.Editor.DTEditorRoute
import Wizard.DocumentTemplateEditors.Routes
import Wizard.DocumentTemplates.Routes
import Wizard.Documents.Routes
import Wizard.KMEditor.Editor.KMEditorRoute
import Wizard.KMEditor.Routes
import Wizard.KnowledgeModels.Routes
import Wizard.Locales.Routes
import Wizard.ProjectActions.Routes
import Wizard.ProjectFiles.Routes
import Wizard.ProjectImporters.Routes
import Wizard.Projects.Detail.ProjectDetailRoute
import Wizard.Projects.Routes
import Wizard.Public.Routes
import Wizard.Registry.Routes
import Wizard.Settings.Routes
import Wizard.Tenants.Routes
import Wizard.Users.Edit.UserEditRoutes as UserEditRoute
import Wizard.Users.Routes


type Route
    = DashboardRoute
    | DevRoute Wizard.Dev.Routes.Route
    | CommentsRoute PaginationQueryString (Maybe String)
    | DocumentsRoute Wizard.Documents.Routes.Route
    | DocumentTemplateEditorsRoute Wizard.DocumentTemplateEditors.Routes.Route
    | DocumentTemplatesRoute Wizard.DocumentTemplates.Routes.Route
    | KMEditorRoute Wizard.KMEditor.Routes.Route
    | KnowledgeModelsRoute Wizard.KnowledgeModels.Routes.Route
    | LocalesRoute Wizard.Locales.Routes.Route
    | ProjectsRoute Wizard.Projects.Routes.Route
    | ProjectActionsRoute Wizard.ProjectActions.Routes.Route
    | ProjectFilesRoute Wizard.ProjectFiles.Routes.Route
    | ProjectImportersRoute Wizard.ProjectImporters.Routes.Route
    | PublicRoute Wizard.Public.Routes.Route
    | RegistryRoute Wizard.Registry.Routes.Route
    | SettingsRoute Wizard.Settings.Routes.Route
    | TenantsRoute Wizard.Tenants.Routes.Route
    | UsersRoute Wizard.Users.Routes.Route
    | NotAllowedRoute
    | NotFoundRoute


commentsRouteResolvedFilterId : String
commentsRouteResolvedFilterId =
    "resolved"


publicHome : Route
publicHome =
    PublicRoute <| Wizard.Public.Routes.LoginRoute Nothing


appHome : Route
appHome =
    DashboardRoute


dashboard : Route
dashboard =
    DashboardRoute


isDashboard : Route -> Bool
isDashboard =
    (==) DashboardRoute



-- Helpers


isSameListingRoute : Route -> Route -> Bool
isSameListingRoute originalRoute nextRoute =
    let
        checkRoute matcher =
            matcher originalRoute && matcher nextRoute
    in
    List.any checkRoute listingRouteMatchers


listingRouteMatchers : List (Route -> Bool)
listingRouteMatchers =
    [ isTenantIndex
    , isDocumentsIndex
    , isDocumentTemplateEditorsIndex
    , isDocumentTemplatesIndex
    , isKmEditorIndex
    , isKnowledgeModelsIndex
    , isLocalesIndex
    , isPersistentCommandsIndex
    , isProjectFilesIndex
    , isProjectImportersIndex
    , isProjectsIndex
    , isUsersIndex
    ]



-- Comments


commentsIndex : Route
commentsIndex =
    CommentsRoute PaginationQueryString.empty (Just "false")


commentsIndexWithFilters : PaginationQueryFilters -> PaginationQueryString -> Route
commentsIndexWithFilters filters pagination =
    CommentsRoute pagination (PaginationQueryFilters.getValue commentsRouteResolvedFilterId filters)



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


documentsIndexWithFilters : Maybe Uuid -> PaginationQueryFilters -> PaginationQueryString -> Route
documentsIndexWithFilters mbQuestionnaireUuid _ pagination =
    DocumentsRoute (Wizard.Documents.Routes.IndexRoute mbQuestionnaireUuid pagination)


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
    documentTemplateEditorDetailFiles


documentTemplateEditorDetailFiles : String -> Route
documentTemplateEditorDetailFiles =
    DocumentTemplateEditorsRoute << flip Wizard.DocumentTemplateEditors.Routes.EditorRoute Wizard.DocumentTemplateEditors.Editor.DTEditorRoute.Files


documentTemplateEditorDetailPreview : String -> Route
documentTemplateEditorDetailPreview =
    DocumentTemplateEditorsRoute << flip Wizard.DocumentTemplateEditors.Routes.EditorRoute Wizard.DocumentTemplateEditors.Editor.DTEditorRoute.Preview


documentTemplateEditorDetailSettings : String -> Route
documentTemplateEditorDetailSettings =
    DocumentTemplateEditorsRoute << flip Wizard.DocumentTemplateEditors.Routes.EditorRoute Wizard.DocumentTemplateEditors.Editor.DTEditorRoute.Settings


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


isDocumentTemplateEditor : String -> Route -> Bool
isDocumentTemplateEditor id route =
    case route of
        DocumentTemplateEditorsRoute (Wizard.DocumentTemplateEditors.Routes.EditorRoute documentTemplateId _) ->
            id == documentTemplateId

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


kmEditorEditorPhases : Uuid -> Route
kmEditorEditorPhases branchUuid =
    KMEditorRoute (Wizard.KMEditor.Routes.EditorRoute branchUuid Wizard.KMEditor.Editor.KMEditorRoute.Phases)


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


knowledgeModelsResourcePage : String -> String -> Route
knowledgeModelsResourcePage kmId resourcePageUuid =
    KnowledgeModelsRoute <| Wizard.KnowledgeModels.Routes.ResourcePageRoute kmId resourcePageUuid


isKnowledgeModelsSubroute : Route -> Bool
isKnowledgeModelsSubroute route =
    case route of
        KnowledgeModelsRoute _ ->
            True

        KMEditorRoute _ ->
            True

        _ ->
            False



-- Project Actions


projectActionsIndex : Route
projectActionsIndex =
    ProjectActionsRoute (Wizard.ProjectActions.Routes.IndexRoute PaginationQueryString.empty)


projectActionsIndexWithFilters : PaginationQueryFilters -> PaginationQueryString -> Route
projectActionsIndexWithFilters _ pagination =
    ProjectActionsRoute (Wizard.ProjectActions.Routes.IndexRoute pagination)


isProjectActionsIndex : Route -> Bool
isProjectActionsIndex route =
    case route of
        ProjectActionsRoute (Wizard.ProjectActions.Routes.IndexRoute _) ->
            True

        _ ->
            False



-- Project Files


projectFilesIndex : Route
projectFilesIndex =
    ProjectFilesRoute (Wizard.ProjectFiles.Routes.IndexRoute PaginationQueryString.empty)


projectFilesIndexWithFilters : PaginationQueryFilters -> PaginationQueryString -> Route
projectFilesIndexWithFilters _ pagination =
    ProjectFilesRoute (Wizard.ProjectFiles.Routes.IndexRoute pagination)


isProjectFilesIndex : Route -> Bool
isProjectFilesIndex route =
    case route of
        ProjectFilesRoute (Wizard.ProjectFiles.Routes.IndexRoute _) ->
            True

        _ ->
            False



-- Project Importers


projectImportersIndex : Route
projectImportersIndex =
    ProjectImportersRoute (Wizard.ProjectImporters.Routes.IndexRoute PaginationQueryString.empty)


projectImportersIndexWithFilters : PaginationQueryFilters -> PaginationQueryString -> Route
projectImportersIndexWithFilters _ pagination =
    ProjectImportersRoute (Wizard.ProjectImporters.Routes.IndexRoute pagination)


isProjectImportersIndex : Route -> Bool
isProjectImportersIndex route =
    case route of
        ProjectImportersRoute (Wizard.ProjectImporters.Routes.IndexRoute _) ->
            True

        _ ->
            False



-- Projects


projectsCreate : Route
projectsCreate =
    ProjectsRoute <| Wizard.Projects.Routes.CreateRoute Nothing Nothing


projectsCreateFromKnowledgeModel : String -> Route
projectsCreateFromKnowledgeModel selectedKnowledgeModel =
    ProjectsRoute <| Wizard.Projects.Routes.CreateRoute Nothing (Just selectedKnowledgeModel)


projectsCreateFromProjectTemplate : Uuid -> Route
projectsCreateFromProjectTemplate selectedProjectTemplate =
    ProjectsRoute <| Wizard.Projects.Routes.CreateRoute (Just selectedProjectTemplate) Nothing


projectsCreateMigration : Uuid -> Route
projectsCreateMigration =
    ProjectsRoute << Wizard.Projects.Routes.CreateMigrationRoute


projectsDetail : Uuid -> Route
projectsDetail uuid =
    ProjectsRoute <| Wizard.Projects.Routes.DetailRoute uuid <| Wizard.Projects.Detail.ProjectDetailRoute.Questionnaire Nothing Nothing


projectsDetailQuestionnaire : Uuid -> Maybe String -> Maybe Uuid -> Route
projectsDetailQuestionnaire uuid mbQuestionPath mbCommentThreadUuid =
    ProjectsRoute <| Wizard.Projects.Routes.DetailRoute uuid <| Wizard.Projects.Detail.ProjectDetailRoute.Questionnaire mbQuestionPath mbCommentThreadUuid


projectsDetailDocuments : Uuid -> Route
projectsDetailDocuments uuid =
    ProjectsRoute <| Wizard.Projects.Routes.DetailRoute uuid <| Wizard.Projects.Detail.ProjectDetailRoute.Documents PaginationQueryString.empty


projectsDetailDocumentsWithFilters : Uuid -> PaginationQueryFilters -> PaginationQueryString -> Route
projectsDetailDocumentsWithFilters uuid _ pagination =
    ProjectsRoute <| Wizard.Projects.Routes.DetailRoute uuid <| Wizard.Projects.Detail.ProjectDetailRoute.Documents pagination


projectsDetailDocumentsNew : Uuid -> Maybe Uuid -> Route
projectsDetailDocumentsNew uuid mbEventUuid =
    ProjectsRoute <| Wizard.Projects.Routes.DetailRoute uuid <| Wizard.Projects.Detail.ProjectDetailRoute.NewDocument mbEventUuid


projectsDetailFilesWithFilters : Uuid -> PaginationQueryFilters -> PaginationQueryString -> Route
projectsDetailFilesWithFilters uuid _ pagination =
    ProjectsRoute <| Wizard.Projects.Routes.DetailRoute uuid <| Wizard.Projects.Detail.ProjectDetailRoute.Files pagination


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


projectsIndex : { a | session : Session, config : BootstrapConfig } -> Route
projectsIndex appState =
    let
        mbUserUuid =
            case appState.config.user of
                Just user ->
                    if user.role == Role.admin then
                        Nothing

                    else
                        Just (Uuid.toString user.uuid)

                Nothing ->
                    Nothing
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


projectsImport : Uuid -> String -> Route
projectsImport uuid importerId =
    ProjectsRoute <| Wizard.Projects.Routes.ImportRoute uuid importerId


isProjectSubroute : Route -> Bool
isProjectSubroute route =
    isDocumentsIndex route
        || (case route of
                ProjectsRoute _ ->
                    True

                ProjectActionsRoute _ ->
                    True

                ProjectImportersRoute _ ->
                    True

                ProjectFilesRoute _ ->
                    True

                _ ->
                    False
           )


projectsFileDownload : Uuid -> Uuid -> Route
projectsFileDownload projectUuid documentUuid =
    ProjectsRoute <| Wizard.Projects.Routes.FileDownloadRoute projectUuid documentUuid


projectDocumentDownload : Uuid -> Uuid -> Route
projectDocumentDownload projectUuid documentUuid =
    ProjectsRoute <| Wizard.Projects.Routes.DocumentDownloadRoute projectUuid documentUuid



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


publicLogoutSuccessful : Route
publicLogoutSuccessful =
    PublicRoute Wizard.Public.Routes.LogoutSuccessful



-- Settings


settingsDefault : Bool -> Route
settingsDefault adminEnabled =
    SettingsRoute (Wizard.Settings.Routes.defaultRoute adminEnabled)


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



-- Tenants


tenantsCreate : Route
tenantsCreate =
    TenantsRoute Wizard.Tenants.Routes.CreateRoute


tenantsDetail : Uuid -> Route
tenantsDetail =
    TenantsRoute << Wizard.Tenants.Routes.DetailRoute


tenantsIndex : Route
tenantsIndex =
    TenantsRoute (Wizard.Tenants.Routes.IndexRoute PaginationQueryString.empty Nothing Nothing)


tenantsIndexWithFilters : PaginationQueryFilters -> PaginationQueryString -> Route
tenantsIndexWithFilters filters pagination =
    TenantsRoute
        (Wizard.Tenants.Routes.IndexRoute pagination
            (PaginationQueryFilters.getValue Wizard.Tenants.Routes.indexRouteEnabledFilterId filters)
            (PaginationQueryFilters.getValue Wizard.Tenants.Routes.indexRouteStatesFilterId filters)
        )


isTenantIndex : Route -> Bool
isTenantIndex route =
    case route of
        TenantsRoute (Wizard.Tenants.Routes.IndexRoute _ _ _) ->
            True

        _ ->
            False



-- Users


usersCreate : Route
usersCreate =
    UsersRoute Wizard.Users.Routes.CreateRoute


usersEdit : UuidOrCurrent -> Route
usersEdit =
    UsersRoute << flip Wizard.Users.Routes.EditRoute UserEditRoute.Profile


usersEditPassword : UuidOrCurrent -> Route
usersEditPassword =
    UsersRoute << flip Wizard.Users.Routes.EditRoute UserEditRoute.Password


usersEditLanguage : UuidOrCurrent -> Route
usersEditLanguage =
    UsersRoute << flip Wizard.Users.Routes.EditRoute UserEditRoute.Language


usersEditTours : UuidOrCurrent -> Route
usersEditTours =
    UsersRoute << flip Wizard.Users.Routes.EditRoute UserEditRoute.Tours


usersEditLanguageCurrent : Route
usersEditLanguageCurrent =
    usersEditLanguage UuidOrCurrent.current


usersEditApiKeys : UuidOrCurrent -> Route
usersEditApiKeys =
    UsersRoute << flip Wizard.Users.Routes.EditRoute UserEditRoute.ApiKeys


usersEditAppKeys : UuidOrCurrent -> Route
usersEditAppKeys =
    UsersRoute << flip Wizard.Users.Routes.EditRoute UserEditRoute.AppKeys


usersEditActiveSessions : UuidOrCurrent -> Route
usersEditActiveSessions =
    UsersRoute << flip Wizard.Users.Routes.EditRoute UserEditRoute.ActiveSessions


usersEditSubmissionSettings : UuidOrCurrent -> Route
usersEditSubmissionSettings =
    UsersRoute << flip Wizard.Users.Routes.EditRoute UserEditRoute.SubmissionSettings


usersEditCurrent : Route
usersEditCurrent =
    usersEdit UuidOrCurrent.current


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


localesIndexWithFilters : PaginationQueryFilters -> PaginationQueryString -> Route
localesIndexWithFilters _ pagination =
    LocalesRoute (Wizard.Locales.Routes.IndexRoute pagination)


localesDetail : String -> Route
localesDetail =
    LocalesRoute << Wizard.Locales.Routes.DetailRoute


isLocalesIndex : Route -> Bool
isLocalesIndex route =
    case route of
        LocalesRoute (Wizard.Locales.Routes.IndexRoute _) ->
            True

        _ ->
            False
