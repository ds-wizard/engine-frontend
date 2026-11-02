module Common.Routes exposing
    ( Route(..)
    , RoutingAppState
    , appHome
    , cmdNavigateToLogin
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
    , documentTemplatesDetailLocales
    , documentTemplatesImport
    , documentTemplatesIndex
    , documentTemplatesIndexWithFilters
    , documentsIndex
    , documentsIndexWithFilters
    , isAllowed
    , isCreateRoute
    , isDashboard
    , isDevOperations
    , isDevSubroute
    , isDocumentTemplateEditor
    , isDocumentTemplateEditorsIndex
    , isDocumentTemplatesIndex
    , isDocumentTemplatesSubroute
    , isDocumentsIndex
    , isKmEditorEditor
    , isKmEditorEditorSettings
    , isKmEditorIndex
    , isKnowledgeModelSecrets
    , isKnowledgeModelsIndex
    , isKnowledgeModelsSubroute
    , isPersistentCommandsIndex
    , isProjectFilesIndex
    , isProjectSubroute
    , isProjectsDetail
    , isProjectsIndex
    , isReachable
    , isSameListingRoute
    , isSettingsRoute
    , isSettingsSubroute
    , isTenantIndex
    , isUserGroupsSubroute
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
    , knowledgeModelSecrets
    , knowledgeModelsCompare
    , knowledgeModelsDetail
    , knowledgeModelsDetailLocales
    , knowledgeModelsImport
    , knowledgeModelsIndex
    , knowledgeModelsIndexWithFilters
    , knowledgeModelsPreview
    , knowledgeModelsResourcePage
    , loginUrl
    , navigate
    , parse
    , persistentCommandsDetail
    , persistentCommandsIndex
    , persistentCommandsIndexWithFilters
    , projectDocumentDownload
    , projectFilesIndex
    , projectFilesIndexWithFilters
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
    , publicForgottenPassword
    , publicHome
    , publicLogin
    , publicLogoutSuccessful
    , publicSignup
    , routeIfAllowed
    , settingsAuthentication
    , settingsDefault
    , settingsLocales
    , settingsLocalesRegistry
    , settingsLocalesUpload
    , settingsLookAndFeel
    , settingsOpenId
    , settingsOpenIdCreate
    , settingsOpenIdDetail
    , settingsPluginSettings
    , settingsRegistry
    , settingsRoleCreate
    , settingsRoleDetail
    , settingsRoles
    , tenantsCreate
    , tenantsDetail
    , tenantsIndex
    , tenantsIndexWithFilters
    , toPath
    , toUrl
    , toUrlIn
    , userGroupsCreate
    , userGroupsDetail
    , userGroupsDetailMembers
    , userGroupsIndex
    , userGroupsIndexWithFilters
    , usersCreate
    , usersEdit
    , usersEditActiveSessions
    , usersEditApiKeys
    , usersEditConnectedAccounts
    , usersEditCurrent
    , usersEditLanguage
    , usersEditLanguageCurrent
    , usersEditPassword
    , usersEditPluginSettings
    , usersEditSubmissionSettings
    , usersEditTours
    , usersIndex
    , usersIndexWithFilters
    )

import Browser.Navigation exposing (Key, pushUrl)
import Common.Api.Models.BootstrapConfig exposing (BootstrapConfig)
import Common.Api.Models.BootstrapConfig.UserConfig as UserConfig
import Common.Data.Feature as Feature exposing (AppStateLike)
import Common.Data.LocaleKind exposing (LocaleKind)
import Common.Data.PaginationQueryFilters as PaginationQueryFilters exposing (PaginationQueryFilters)
import Common.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Common.Data.Session exposing (Session)
import Common.Data.UuidOrCurrent as UuidOrCurrent exposing (UuidOrCurrent)
import Common.Data.WizardRolePermission as RolePermission
import Common.Pages.Dev.Routes
import Common.Pages.DocumentTemplateEditors.Editor.Routes
import Common.Pages.DocumentTemplateEditors.Routes
import Common.Pages.DocumentTemplates.Detail.Routes
import Common.Pages.DocumentTemplates.Routes
import Common.Pages.Documents.Routes
import Common.Pages.KMEditor.Editor.Routes
import Common.Pages.KMEditor.Routes
import Common.Pages.KnowledgeModels.Detail.Routes
import Common.Pages.KnowledgeModels.Routes
import Common.Pages.ProjectFiles.Routes
import Common.Pages.Projects.Detail.Routes
import Common.Pages.Projects.Routes
import Common.Pages.Public.Routes
import Common.Pages.Settings.Routes
import Common.Pages.Tenants.Routes
import Common.Pages.UserGroups.Routes
import Common.Pages.Users.Edit.Routes as UserEditRoute
import Common.Pages.Users.Routes
import Common.Plugins.PluginMetadata exposing (PluginMetadata)
import Flip exposing (flip)
import Url exposing (Url)
import Url.Parser exposing ((</>), Parser, map, oneOf, s)
import Url.Parser.Query as Query
import Uuid exposing (Uuid)


type Route
    = DashboardRoute
    | DevRoute Common.Pages.Dev.Routes.Route
    | CommentsRoute PaginationQueryString (Maybe String)
    | DocumentsRoute Common.Pages.Documents.Routes.Route
    | DocumentTemplateEditorsRoute Common.Pages.DocumentTemplateEditors.Routes.Route
    | DocumentTemplatesRoute Common.Pages.DocumentTemplates.Routes.Route
    | KMEditorRoute Common.Pages.KMEditor.Routes.Route
    | KnowledgeModelSecretsRoute
    | KnowledgeModelsRoute Common.Pages.KnowledgeModels.Routes.Route
    | UserGroupsRoute Common.Pages.UserGroups.Routes.Route
    | ProjectsRoute Common.Pages.Projects.Routes.Route
    | ProjectFilesRoute Common.Pages.ProjectFiles.Routes.Route
    | PublicRoute Common.Pages.Public.Routes.Route
    | SettingsRoute Common.Pages.Settings.Routes.Route
    | TenantsRoute Common.Pages.Tenants.Routes.Route
    | UsersRoute Common.Pages.Users.Routes.Route
    | NotAllowedRoute
    | NotFoundRoute


commentsRouteResolvedFilterId : String
commentsRouteResolvedFilterId =
    "resolved"


publicHome : Route
publicHome =
    PublicRoute <| Common.Pages.Public.Routes.LoginRoute Nothing


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


{-| The pages whose one purpose is a form that creates something. They open with
their first field focused rather than their heading (`Common.Components.Form`).
-}
isCreateRoute : Route -> Bool
isCreateRoute route =
    case route of
        KMEditorRoute (Common.Pages.KMEditor.Routes.CreateRoute _ _) ->
            True

        DocumentTemplateEditorsRoute (Common.Pages.DocumentTemplateEditors.Routes.CreateRoute _ _) ->
            True

        UserGroupsRoute Common.Pages.UserGroups.Routes.Create ->
            True

        ProjectsRoute (Common.Pages.Projects.Routes.CreateRoute _ _) ->
            True

        ProjectsRoute (Common.Pages.Projects.Routes.DetailRoute _ (Common.Pages.Projects.Detail.Routes.NewDocument _)) ->
            True

        SettingsRoute Common.Pages.Settings.Routes.OpenIdCreateRoute ->
            True

        SettingsRoute Common.Pages.Settings.Routes.RoleCreateRoute ->
            True

        TenantsRoute Common.Pages.Tenants.Routes.CreateRoute ->
            True

        UsersRoute Common.Pages.Users.Routes.CreateRoute ->
            True

        _ ->
            False


isSameListingRoute : Route -> Route -> Bool
isSameListingRoute originalRoute nextRoute =
    let
        checkRoute matcher =
            matcher originalRoute && matcher nextRoute
    in
    List.any checkRoute listingRouteMatchers
        || isSameUserGroups originalRoute nextRoute
        || isSameKnowledgeModelDetail originalRoute nextRoute
        || isSameDocumentTemplateDetail originalRoute nextRoute
        || isSameProjectDocuments originalRoute nextRoute
        || isSameProjectFiles originalRoute nextRoute


{-| The documents tab of a project detail is a listing: searching, sorting and
paging it replace the URL, and that must not pull the page out from under the
listing. Two documents routes for the same project are therefore the same
context — while every other pair of project detail routes is not, because moving
between the tabs is what loads each tab's data.
-}
isSameProjectDocuments : Route -> Route -> Bool
isSameProjectDocuments originalRoute nextRoute =
    case ( originalRoute, nextRoute ) of
        ( ProjectsRoute (Common.Pages.Projects.Routes.DetailRoute uuid1 (Common.Pages.Projects.Detail.Routes.Documents _)), ProjectsRoute (Common.Pages.Projects.Routes.DetailRoute uuid2 (Common.Pages.Projects.Detail.Routes.Documents _)) ) ->
            uuid1 == uuid2

        _ ->
            False


{-| The files tab of a project detail is a listing too, and the same reasoning
applies: two files routes for the same project are the same context, so
searching, sorting or paging does not re-run the page's `init`.
-}
isSameProjectFiles : Route -> Route -> Bool
isSameProjectFiles originalRoute nextRoute =
    case ( originalRoute, nextRoute ) of
        ( ProjectsRoute (Common.Pages.Projects.Routes.DetailRoute uuid1 (Common.Pages.Projects.Detail.Routes.Files _)), ProjectsRoute (Common.Pages.Projects.Routes.DetailRoute uuid2 (Common.Pages.Projects.Detail.Routes.Files _)) ) ->
            uuid1 == uuid2

        _ ->
            False


{-| The knowledge model detail page keeps its data in the page model and switches
between tabs (readme, locales) only by changing the route. Two detail routes for
the same knowledge model are therefore treated as the same context, so switching
tabs does not reload the whole page.
-}
isSameKnowledgeModelDetail : Route -> Route -> Bool
isSameKnowledgeModelDetail originalRoute nextRoute =
    case ( originalRoute, nextRoute ) of
        ( KnowledgeModelsRoute (Common.Pages.KnowledgeModels.Routes.DetailRoute uuid1 _), KnowledgeModelsRoute (Common.Pages.KnowledgeModels.Routes.DetailRoute uuid2 _) ) ->
            uuid1 == uuid2

        _ ->
            False


{-| The document template detail page keeps its data in the page model and switches
between tabs (readme, locales) only by changing the route. Two detail routes for
the same document template are therefore treated as the same context, so switching
tabs does not reload the whole page.
-}
isSameDocumentTemplateDetail : Route -> Route -> Bool
isSameDocumentTemplateDetail originalRoute nextRoute =
    case ( originalRoute, nextRoute ) of
        ( DocumentTemplatesRoute (Common.Pages.DocumentTemplates.Routes.DetailRoute uuid1 _), DocumentTemplatesRoute (Common.Pages.DocumentTemplates.Routes.DetailRoute uuid2 _) ) ->
            uuid1 == uuid2

        _ ->
            False


listingRouteMatchers : List (Route -> Bool)
listingRouteMatchers =
    [ isCommentsIndex
    , isTenantIndex
    , isDocumentsIndex
    , isDocumentTemplateEditorsIndex
    , isDocumentTemplatesIndex
    , isKmEditorIndex
    , isKnowledgeModelsIndex
    , isPersistentCommandsIndex
    , isProjectFilesIndex
    , isProjectsIndex
    , isUsersIndex
    ]



-- Comments


commentsIndex : Route
commentsIndex =
    CommentsRoute PaginationQueryString.empty (Just "false")


isCommentsIndex : Route -> Bool
isCommentsIndex route =
    case route of
        CommentsRoute _ _ ->
            True

        _ ->
            False


commentsIndexWithFilters : PaginationQueryFilters -> PaginationQueryString -> Route
commentsIndexWithFilters filters pagination =
    CommentsRoute pagination (PaginationQueryFilters.getValue commentsRouteResolvedFilterId filters)



-- Dev


devOperations : Route
devOperations =
    DevRoute (Common.Pages.Dev.Routes.OperationsRoute Nothing)


isDevOperations : Route -> Bool
isDevOperations route =
    case route of
        DevRoute (Common.Pages.Dev.Routes.OperationsRoute _) ->
            True

        _ ->
            False


persistentCommandsIndex : Route
persistentCommandsIndex =
    DevRoute (Common.Pages.Dev.Routes.PersistentCommandsIndex PaginationQueryString.empty Nothing)


isPersistentCommandsIndex : Route -> Bool
isPersistentCommandsIndex route =
    case route of
        DevRoute (Common.Pages.Dev.Routes.PersistentCommandsIndex _ _) ->
            True

        _ ->
            False


persistentCommandsIndexWithFilters : PaginationQueryFilters -> PaginationQueryString -> Route
persistentCommandsIndexWithFilters filters pagination =
    DevRoute
        (Common.Pages.Dev.Routes.PersistentCommandsIndex pagination
            (PaginationQueryFilters.getValue Common.Pages.Dev.Routes.persistentCommandIndexRouteStateFilterId filters)
        )


persistentCommandsDetail : Uuid -> Route
persistentCommandsDetail =
    DevRoute << Common.Pages.Dev.Routes.PersistentCommandsDetail


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
    DocumentsRoute (Common.Pages.Documents.Routes.IndexRoute Nothing PaginationQueryString.empty)


documentsIndexWithFilters : Maybe Uuid -> PaginationQueryFilters -> PaginationQueryString -> Route
documentsIndexWithFilters mbQuestionnaireUuid _ pagination =
    DocumentsRoute (Common.Pages.Documents.Routes.IndexRoute mbQuestionnaireUuid pagination)


isDocumentsIndex : Route -> Bool
isDocumentsIndex route =
    case route of
        DocumentsRoute (Common.Pages.Documents.Routes.IndexRoute _ _) ->
            True

        _ ->
            False



-- Document Templates


documentTemplatesDetail : Uuid -> Route
documentTemplatesDetail uuid =
    DocumentTemplatesRoute <| Common.Pages.DocumentTemplates.Routes.DetailRoute uuid Common.Pages.DocumentTemplates.Detail.Routes.Readme


documentTemplatesDetailLocales : Uuid -> Route
documentTemplatesDetailLocales uuid =
    DocumentTemplatesRoute <| Common.Pages.DocumentTemplates.Routes.DetailRoute uuid Common.Pages.DocumentTemplates.Detail.Routes.Locales


documentTemplatesImport : Maybe String -> Route
documentTemplatesImport =
    DocumentTemplatesRoute << Common.Pages.DocumentTemplates.Routes.ImportRoute


documentTemplatesIndex : Route
documentTemplatesIndex =
    DocumentTemplatesRoute (Common.Pages.DocumentTemplates.Routes.IndexRoute PaginationQueryString.empty)


documentTemplatesIndexWithFilters : PaginationQueryFilters -> PaginationQueryString -> Route
documentTemplatesIndexWithFilters _ pagination =
    DocumentTemplatesRoute (Common.Pages.DocumentTemplates.Routes.IndexRoute pagination)


isDocumentTemplatesIndex : Route -> Bool
isDocumentTemplatesIndex route =
    case route of
        DocumentTemplatesRoute (Common.Pages.DocumentTemplates.Routes.IndexRoute _) ->
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


documentTemplateEditorCreate : Maybe Uuid -> Maybe Bool -> Route
documentTemplateEditorCreate mbBasedOn mbEdit =
    DocumentTemplateEditorsRoute <| Common.Pages.DocumentTemplateEditors.Routes.CreateRoute mbBasedOn mbEdit


documentTemplateEditorDetail : Uuid -> Route
documentTemplateEditorDetail =
    documentTemplateEditorDetailFiles


documentTemplateEditorDetailFiles : Uuid -> Route
documentTemplateEditorDetailFiles =
    DocumentTemplateEditorsRoute << flip Common.Pages.DocumentTemplateEditors.Routes.EditorRoute Common.Pages.DocumentTemplateEditors.Editor.Routes.Files


documentTemplateEditorDetailPreview : Uuid -> Route
documentTemplateEditorDetailPreview =
    DocumentTemplateEditorsRoute << flip Common.Pages.DocumentTemplateEditors.Routes.EditorRoute Common.Pages.DocumentTemplateEditors.Editor.Routes.Preview


{-| One section of the editor's settings, which is where the menu beside them
points.
-}
documentTemplateEditorDetailSettings : Common.Pages.DocumentTemplateEditors.Editor.Routes.DTSettingsSection -> Uuid -> Route
documentTemplateEditorDetailSettings section =
    DocumentTemplateEditorsRoute << flip Common.Pages.DocumentTemplateEditors.Routes.EditorRoute (Common.Pages.DocumentTemplateEditors.Editor.Routes.Settings section)


documentTemplateEditorsIndex : Route
documentTemplateEditorsIndex =
    DocumentTemplateEditorsRoute (Common.Pages.DocumentTemplateEditors.Routes.IndexRoute PaginationQueryString.empty)


documentTemplateEditorsIndexWithFilters : PaginationQueryFilters -> PaginationQueryString -> Route
documentTemplateEditorsIndexWithFilters _ pagination =
    DocumentTemplateEditorsRoute (Common.Pages.DocumentTemplateEditors.Routes.IndexRoute pagination)


isDocumentTemplateEditorsIndex : Route -> Bool
isDocumentTemplateEditorsIndex route =
    case route of
        DocumentTemplateEditorsRoute (Common.Pages.DocumentTemplateEditors.Routes.IndexRoute _) ->
            True

        _ ->
            False


isDocumentTemplateEditor : Uuid -> Route -> Bool
isDocumentTemplateEditor uuid route =
    case route of
        DocumentTemplateEditorsRoute (Common.Pages.DocumentTemplateEditors.Routes.EditorRoute documentTemplateUuid _) ->
            uuid == documentTemplateUuid

        _ ->
            False



-- KM Editor


kmEditorCreate : Maybe Uuid -> Maybe Bool -> Route
kmEditorCreate mbKmUuid mbEdit =
    KMEditorRoute <| Common.Pages.KMEditor.Routes.CreateRoute mbKmUuid mbEdit


kmEditorEditor : Uuid -> Maybe Uuid -> Route
kmEditorEditor kmEditorUuid mbEntityUuid =
    KMEditorRoute (Common.Pages.KMEditor.Routes.EditorRoute kmEditorUuid (Common.Pages.KMEditor.Editor.Routes.Edit mbEntityUuid))


isKmEditorEditor : Uuid -> Route -> Bool
isKmEditorEditor uuid route =
    case route of
        KMEditorRoute (Common.Pages.KMEditor.Routes.EditorRoute editorUuid _) ->
            uuid == editorUuid

        _ ->
            False


{-| This editor's Settings tab and no other page, which is where a form full of
unsaved settings may still be left.
-}
isKmEditorEditorSettings : Uuid -> Route -> Bool
isKmEditorEditorSettings uuid route =
    route == kmEditorEditorSettings uuid


kmEditorEditorPhases : Uuid -> Route
kmEditorEditorPhases kmEditorUuid =
    KMEditorRoute (Common.Pages.KMEditor.Routes.EditorRoute kmEditorUuid Common.Pages.KMEditor.Editor.Routes.Phases)


kmEditorEditorQuestionTags : Uuid -> Route
kmEditorEditorQuestionTags kmEditorUuid =
    KMEditorRoute (Common.Pages.KMEditor.Routes.EditorRoute kmEditorUuid Common.Pages.KMEditor.Editor.Routes.QuestionTags)


kmEditorEditorPreview : Uuid -> Route
kmEditorEditorPreview kmEditorUuid =
    KMEditorRoute (Common.Pages.KMEditor.Routes.EditorRoute kmEditorUuid Common.Pages.KMEditor.Editor.Routes.Preview)


kmEditorEditorSettings : Uuid -> Route
kmEditorEditorSettings kmEditorUuid =
    KMEditorRoute (Common.Pages.KMEditor.Routes.EditorRoute kmEditorUuid Common.Pages.KMEditor.Editor.Routes.Settings)


kmEditorIndex : Route
kmEditorIndex =
    KMEditorRoute (Common.Pages.KMEditor.Routes.IndexRoute PaginationQueryString.empty)


kmEditorIndexWithFilters : PaginationQueryFilters -> PaginationQueryString -> Route
kmEditorIndexWithFilters _ pagination =
    KMEditorRoute (Common.Pages.KMEditor.Routes.IndexRoute pagination)


isKmEditorIndex : Route -> Bool
isKmEditorIndex route =
    case route of
        KMEditorRoute (Common.Pages.KMEditor.Routes.IndexRoute _) ->
            True

        _ ->
            False


kmEditorMigration : Uuid -> Route
kmEditorMigration =
    KMEditorRoute << Common.Pages.KMEditor.Routes.MigrationRoute


kmEditorPublish : Uuid -> Route
kmEditorPublish =
    KMEditorRoute << Common.Pages.KMEditor.Routes.PublishRoute



-- Knowledge Models


knowledgeModelsCompare : Maybe Uuid -> Route
knowledgeModelsCompare mbLeftKmUuid =
    KnowledgeModelsRoute <| Common.Pages.KnowledgeModels.Routes.CompareRoute mbLeftKmUuid


knowledgeModelsDetail : Uuid -> Route
knowledgeModelsDetail uuid =
    KnowledgeModelsRoute <| Common.Pages.KnowledgeModels.Routes.DetailRoute uuid Common.Pages.KnowledgeModels.Detail.Routes.Readme


knowledgeModelsDetailLocales : Uuid -> Route
knowledgeModelsDetailLocales uuid =
    KnowledgeModelsRoute <| Common.Pages.KnowledgeModels.Routes.DetailRoute uuid Common.Pages.KnowledgeModels.Detail.Routes.Locales


knowledgeModelsImport : Maybe String -> Route
knowledgeModelsImport =
    KnowledgeModelsRoute << Common.Pages.KnowledgeModels.Routes.ImportRoute


knowledgeModelsIndex : Route
knowledgeModelsIndex =
    KnowledgeModelsRoute (Common.Pages.KnowledgeModels.Routes.IndexRoute PaginationQueryString.empty)


knowledgeModelsIndexWithFilters : PaginationQueryFilters -> PaginationQueryString -> Route
knowledgeModelsIndexWithFilters _ pagination =
    KnowledgeModelsRoute (Common.Pages.KnowledgeModels.Routes.IndexRoute pagination)


isKnowledgeModelsIndex : Route -> Bool
isKnowledgeModelsIndex route =
    case route of
        KnowledgeModelsRoute (Common.Pages.KnowledgeModels.Routes.IndexRoute _) ->
            True

        _ ->
            False


knowledgeModelsPreview : Uuid -> Maybe String -> Route
knowledgeModelsPreview kmUuid mbQuestionUuid =
    KnowledgeModelsRoute <| Common.Pages.KnowledgeModels.Routes.PreviewRoute kmUuid mbQuestionUuid


knowledgeModelsResourcePage : Uuid -> String -> Route
knowledgeModelsResourcePage kmUuid resourcePageUuid =
    KnowledgeModelsRoute <| Common.Pages.KnowledgeModels.Routes.ResourcePageRoute kmUuid resourcePageUuid


isKnowledgeModelsSubroute : Route -> Bool
isKnowledgeModelsSubroute route =
    case route of
        KnowledgeModelsRoute _ ->
            True

        KMEditorRoute _ ->
            True

        KnowledgeModelSecretsRoute ->
            True

        _ ->
            False



-- Knowledge Model Secrets


knowledgeModelSecrets : Route
knowledgeModelSecrets =
    KnowledgeModelSecretsRoute


isKnowledgeModelSecrets : Route -> Bool
isKnowledgeModelSecrets route =
    case route of
        KnowledgeModelSecretsRoute ->
            True

        _ ->
            False



-- Project Files


projectFilesIndex : Route
projectFilesIndex =
    ProjectFilesRoute (Common.Pages.ProjectFiles.Routes.IndexRoute PaginationQueryString.empty)


projectFilesIndexWithFilters : PaginationQueryFilters -> PaginationQueryString -> Route
projectFilesIndexWithFilters _ pagination =
    ProjectFilesRoute (Common.Pages.ProjectFiles.Routes.IndexRoute pagination)


isProjectFilesIndex : Route -> Bool
isProjectFilesIndex route =
    case route of
        ProjectFilesRoute (Common.Pages.ProjectFiles.Routes.IndexRoute _) ->
            True

        _ ->
            False



-- Projects


projectsCreate : Route
projectsCreate =
    ProjectsRoute <| Common.Pages.Projects.Routes.CreateRoute Nothing Nothing


projectsCreateFromKnowledgeModel : Uuid -> Route
projectsCreateFromKnowledgeModel selectedKnowledgeModel =
    ProjectsRoute <| Common.Pages.Projects.Routes.CreateRoute Nothing (Just selectedKnowledgeModel)


projectsCreateFromProjectTemplate : Uuid -> Route
projectsCreateFromProjectTemplate selectedProjectTemplate =
    ProjectsRoute <| Common.Pages.Projects.Routes.CreateRoute (Just selectedProjectTemplate) Nothing


projectsCreateMigration : Uuid -> Route
projectsCreateMigration =
    ProjectsRoute << Common.Pages.Projects.Routes.CreateMigrationRoute


projectsDetail : Uuid -> Route
projectsDetail uuid =
    ProjectsRoute <| Common.Pages.Projects.Routes.DetailRoute uuid <| Common.Pages.Projects.Detail.Routes.Questionnaire Nothing Nothing


projectsDetailQuestionnaire : Uuid -> Maybe String -> Maybe Uuid -> Route
projectsDetailQuestionnaire uuid mbQuestionPath mbCommentThreadUuid =
    ProjectsRoute <| Common.Pages.Projects.Routes.DetailRoute uuid <| Common.Pages.Projects.Detail.Routes.Questionnaire mbQuestionPath mbCommentThreadUuid


projectsDetailDocuments : Uuid -> Route
projectsDetailDocuments uuid =
    ProjectsRoute <| Common.Pages.Projects.Routes.DetailRoute uuid <| Common.Pages.Projects.Detail.Routes.Documents PaginationQueryString.empty


projectsDetailDocumentsWithFilters : Uuid -> PaginationQueryFilters -> PaginationQueryString -> Route
projectsDetailDocumentsWithFilters uuid _ pagination =
    ProjectsRoute <| Common.Pages.Projects.Routes.DetailRoute uuid <| Common.Pages.Projects.Detail.Routes.Documents pagination


projectsDetailDocumentsNew : Uuid -> Maybe Uuid -> Route
projectsDetailDocumentsNew uuid mbEventUuid =
    ProjectsRoute <| Common.Pages.Projects.Routes.DetailRoute uuid <| Common.Pages.Projects.Detail.Routes.NewDocument mbEventUuid


projectsDetailFilesWithFilters : Uuid -> PaginationQueryFilters -> PaginationQueryString -> Route
projectsDetailFilesWithFilters uuid _ pagination =
    ProjectsRoute <| Common.Pages.Projects.Routes.DetailRoute uuid <| Common.Pages.Projects.Detail.Routes.Files pagination


projectsDetailSettings : Uuid -> Route
projectsDetailSettings uuid =
    ProjectsRoute <| Common.Pages.Projects.Routes.DetailRoute uuid <| Common.Pages.Projects.Detail.Routes.Settings


isProjectsDetail : Uuid -> Route -> Bool
isProjectsDetail uuid route =
    case route of
        ProjectsRoute (Common.Pages.Projects.Routes.DetailRoute projectUuid _) ->
            uuid == projectUuid

        _ ->
            False


projectsIndex : { a | session : Session, config : BootstrapConfig } -> Route
projectsIndex appState =
    let
        mbUserUuid =
            case appState.config.user of
                Just user ->
                    if UserConfig.hasPerm RolePermission.projectsView user then
                        Nothing

                    else
                        Just (Uuid.toString user.uuid)

                Nothing ->
                    Nothing
    in
    ProjectsRoute (Common.Pages.Projects.Routes.IndexRoute PaginationQueryString.empty Nothing mbUserUuid Nothing Nothing Nothing Nothing Nothing Nothing Nothing)


projectsIndexWithFilters : PaginationQueryFilters -> PaginationQueryString -> Route
projectsIndexWithFilters filters pagination =
    ProjectsRoute
        (Common.Pages.Projects.Routes.IndexRoute pagination
            (PaginationQueryFilters.getValue Common.Pages.Projects.Routes.indexRouteIsTemplateFilterId filters)
            (PaginationQueryFilters.getValue Common.Pages.Projects.Routes.indexRouteUsersFilterId filters)
            (PaginationQueryFilters.getOp Common.Pages.Projects.Routes.indexRouteUsersFilterId filters)
            (PaginationQueryFilters.getValue Common.Pages.Projects.Routes.indexRouteUserGroupsFilterId filters)
            (PaginationQueryFilters.getOp Common.Pages.Projects.Routes.indexRouteUserGroupsFilterId filters)
            (PaginationQueryFilters.getValue Common.Pages.Projects.Routes.indexRouteProjectTagsFilterId filters)
            (PaginationQueryFilters.getOp Common.Pages.Projects.Routes.indexRouteProjectTagsFilterId filters)
            (PaginationQueryFilters.getValue Common.Pages.Projects.Routes.indexRouteKnowledgeModelPackagesFilterId filters)
            (PaginationQueryFilters.getOp Common.Pages.Projects.Routes.indexRouteKnowledgeModelPackagesFilterId filters)
        )


isProjectsIndex : Route -> Bool
isProjectsIndex route =
    case route of
        ProjectsRoute (Common.Pages.Projects.Routes.IndexRoute _ _ _ _ _ _ _ _ _ _) ->
            True

        _ ->
            False


projectsImport : Uuid -> String -> Route
projectsImport uuid importerUrl =
    ProjectsRoute <| Common.Pages.Projects.Routes.ImportRoute uuid importerUrl


isProjectSubroute : Route -> Bool
isProjectSubroute route =
    isDocumentsIndex route
        || (case route of
                ProjectsRoute _ ->
                    True

                ProjectFilesRoute _ ->
                    True

                _ ->
                    False
           )


projectsFileDownload : Uuid -> Uuid -> Route
projectsFileDownload projectUuid documentUuid =
    ProjectsRoute <| Common.Pages.Projects.Routes.FileDownloadRoute projectUuid documentUuid


projectDocumentDownload : Uuid -> Uuid -> Route
projectDocumentDownload projectUuid documentUuid =
    ProjectsRoute <| Common.Pages.Projects.Routes.DocumentDownloadRoute projectUuid documentUuid



-- Public


publicForgottenPassword : Route
publicForgottenPassword =
    PublicRoute Common.Pages.Public.Routes.ForgottenPasswordRoute


publicLogin : Maybe String -> Route
publicLogin originalUrl =
    PublicRoute <| Common.Pages.Public.Routes.LoginRoute originalUrl


publicSignup : Route
publicSignup =
    PublicRoute Common.Pages.Public.Routes.SignupRoute


publicLogoutSuccessful : Route
publicLogoutSuccessful =
    PublicRoute Common.Pages.Public.Routes.LogoutSuccessful



-- Settings


settingsDefault : AppStateLike a -> Route
settingsDefault appState =
    SettingsRoute (Common.Pages.Settings.Routes.defaultRoute appState)


isSettingsRoute : Route -> Bool
isSettingsRoute route =
    case route of
        SettingsRoute _ ->
            True

        _ ->
            False


{-| The settings and the users, what the administration in the menu holds.
Editing one's own account is not administration: it is under no entry.
-}
isSettingsSubroute : Route -> Bool
isSettingsSubroute route =
    case route of
        SettingsRoute _ ->
            True

        UsersRoute (Common.Pages.Users.Routes.EditRoute uuidOrCurrent _) ->
            not (UuidOrCurrent.isCurrent uuidOrCurrent)

        UsersRoute _ ->
            True

        _ ->
            False


settingsAuthentication : Route
settingsAuthentication =
    SettingsRoute Common.Pages.Settings.Routes.AuthenticationRoute


settingsLookAndFeel : Route
settingsLookAndFeel =
    SettingsRoute Common.Pages.Settings.Routes.LookAndFeelRoute


settingsOpenId : Route
settingsOpenId =
    SettingsRoute Common.Pages.Settings.Routes.OpenIdRoute


settingsOpenIdCreate : Route
settingsOpenIdCreate =
    SettingsRoute Common.Pages.Settings.Routes.OpenIdCreateRoute


settingsOpenIdDetail : Uuid -> Route
settingsOpenIdDetail =
    SettingsRoute << Common.Pages.Settings.Routes.OpenIdDetailRoute


settingsRoleCreate : Route
settingsRoleCreate =
    SettingsRoute Common.Pages.Settings.Routes.RoleCreateRoute


settingsRoleDetail : Uuid -> Route
settingsRoleDetail =
    SettingsRoute << Common.Pages.Settings.Routes.RoleDetailRoute


settingsRoles : Route
settingsRoles =
    SettingsRoute Common.Pages.Settings.Routes.RolesRoute


settingsRegistry : Route
settingsRegistry =
    SettingsRoute Common.Pages.Settings.Routes.RegistryRoute


settingsPluginSettings : Uuid -> Route
settingsPluginSettings pluginUuid =
    SettingsRoute (Common.Pages.Settings.Routes.PluginSettingsRoute pluginUuid)



-- Tenants


tenantsCreate : Route
tenantsCreate =
    TenantsRoute Common.Pages.Tenants.Routes.CreateRoute


tenantsDetail : Uuid -> Route
tenantsDetail =
    TenantsRoute << Common.Pages.Tenants.Routes.DetailRoute


tenantsIndex : Route
tenantsIndex =
    TenantsRoute (Common.Pages.Tenants.Routes.IndexRoute PaginationQueryString.empty Nothing Nothing)


tenantsIndexWithFilters : PaginationQueryFilters -> PaginationQueryString -> Route
tenantsIndexWithFilters filters pagination =
    TenantsRoute
        (Common.Pages.Tenants.Routes.IndexRoute pagination
            (PaginationQueryFilters.getValue Common.Pages.Tenants.Routes.indexRouteEnabledFilterId filters)
            (PaginationQueryFilters.getValue Common.Pages.Tenants.Routes.indexRouteStatesFilterId filters)
        )


isTenantIndex : Route -> Bool
isTenantIndex route =
    case route of
        TenantsRoute (Common.Pages.Tenants.Routes.IndexRoute _ _ _) ->
            True

        _ ->
            False



-- Users


usersCreate : Route
usersCreate =
    UsersRoute Common.Pages.Users.Routes.CreateRoute


usersEdit : UuidOrCurrent -> Route
usersEdit =
    UsersRoute << flip Common.Pages.Users.Routes.EditRoute UserEditRoute.Profile


usersEditPassword : UuidOrCurrent -> Route
usersEditPassword =
    UsersRoute << flip Common.Pages.Users.Routes.EditRoute UserEditRoute.Password


usersEditConnectedAccounts : Route
usersEditConnectedAccounts =
    UsersRoute (Common.Pages.Users.Routes.EditRoute UuidOrCurrent.current UserEditRoute.ConnectedAccounts)


usersEditLanguage : UuidOrCurrent -> Route
usersEditLanguage =
    UsersRoute << flip Common.Pages.Users.Routes.EditRoute UserEditRoute.Language


usersEditTours : UuidOrCurrent -> Route
usersEditTours =
    UsersRoute << flip Common.Pages.Users.Routes.EditRoute UserEditRoute.Tours


usersEditLanguageCurrent : Route
usersEditLanguageCurrent =
    usersEditLanguage UuidOrCurrent.current


usersEditApiKeys : UuidOrCurrent -> Route
usersEditApiKeys =
    UsersRoute << flip Common.Pages.Users.Routes.EditRoute UserEditRoute.ApiKeys


usersEditActiveSessions : UuidOrCurrent -> Route
usersEditActiveSessions =
    UsersRoute << flip Common.Pages.Users.Routes.EditRoute UserEditRoute.ActiveSessions


usersEditSubmissionSettings : UuidOrCurrent -> Route
usersEditSubmissionSettings =
    UsersRoute << flip Common.Pages.Users.Routes.EditRoute UserEditRoute.SubmissionSettings


usersEditPluginSettings : UuidOrCurrent -> Uuid -> Route
usersEditPluginSettings uuidOrCurrent pluginUuid =
    UsersRoute (Common.Pages.Users.Routes.EditRoute uuidOrCurrent (UserEditRoute.PluginSettings pluginUuid))


usersEditCurrent : Route
usersEditCurrent =
    usersEdit UuidOrCurrent.current


usersIndex : Route
usersIndex =
    UsersRoute (Common.Pages.Users.Routes.IndexRoute PaginationQueryString.empty Nothing)


usersIndexWithFilters : PaginationQueryFilters -> PaginationQueryString -> Route
usersIndexWithFilters filters pagination =
    UsersRoute
        (Common.Pages.Users.Routes.IndexRoute pagination
            (PaginationQueryFilters.getValue Common.Pages.Users.Routes.indexRouteRoleFilterId filters)
        )


isUsersIndex : Route -> Bool
isUsersIndex route =
    case route of
        UsersRoute (Common.Pages.Users.Routes.IndexRoute _ _) ->
            True

        _ ->
            False



-- User groups


userGroupsIndex : Route
userGroupsIndex =
    UserGroupsRoute (Common.Pages.UserGroups.Routes.Index PaginationQueryString.empty Nothing)


userGroupsIndexWithFilters : PaginationQueryFilters -> PaginationQueryString -> Route
userGroupsIndexWithFilters filters pagination =
    UserGroupsRoute (Common.Pages.UserGroups.Routes.Index pagination (PaginationQueryFilters.getValue Common.Pages.UserGroups.Routes.indexRouteMembershipFilterId filters))


userGroupsCreate : Route
userGroupsCreate =
    UserGroupsRoute Common.Pages.UserGroups.Routes.Create


userGroupsDetail : Uuid -> Route
userGroupsDetail uuid =
    UserGroupsRoute (Common.Pages.UserGroups.Routes.Detail uuid Common.Pages.UserGroups.Routes.Info)


userGroupsDetailMembers : Uuid -> Route
userGroupsDetailMembers uuid =
    UserGroupsRoute (Common.Pages.UserGroups.Routes.Detail uuid Common.Pages.UserGroups.Routes.Members)


isUserGroupsSubroute : Route -> Bool
isUserGroupsSubroute route =
    case route of
        UserGroupsRoute _ ->
            True

        _ ->
            False


{-| Another page or filter of the list keeps the page and its focus.
-}
isSameUserGroups : Route -> Route -> Bool
isSameUserGroups originalRoute nextRoute =
    case ( originalRoute, nextRoute ) of
        ( UserGroupsRoute a, UserGroupsRoute b ) ->
            Common.Pages.UserGroups.Routes.isSameListing a b

        _ ->
            False



-- Locales


{-| The settings section of the translations of a kind, and its upload
and registry pages.
-}
settingsLocales : LocaleKind -> Route
settingsLocales kind =
    SettingsRoute (Common.Pages.Settings.Routes.LocalesRoute kind Common.Pages.Settings.Routes.LocalesListPage)


settingsLocalesUpload : LocaleKind -> Route
settingsLocalesUpload kind =
    SettingsRoute (Common.Pages.Settings.Routes.LocalesRoute kind Common.Pages.Settings.Routes.LocalesUploadPage)


settingsLocalesRegistry : LocaleKind -> Route
settingsLocalesRegistry kind =
    SettingsRoute (Common.Pages.Settings.Routes.LocalesRoute kind Common.Pages.Settings.Routes.LocalesRegistryPage)



-- Routing


type alias RoutingAppState a =
    AppStateLike { a | pluginMetadata : List PluginMetadata }


matchers : RoutingAppState a -> Parser (Route -> b) b
matchers appState =
    let
        commentsIndexRoute pqs mbResolved =
            CommentsRoute pqs mbResolved

        parsers =
            Common.Pages.Dev.Routes.parsers DevRoute
                ++ Common.Pages.Tenants.Routes.parsers TenantsRoute
                ++ Common.Pages.Documents.Routes.parsers DocumentsRoute
                ++ Common.Pages.DocumentTemplateEditors.Routes.parsers DocumentTemplateEditorsRoute
                ++ Common.Pages.DocumentTemplates.Routes.parsers DocumentTemplatesRoute
                ++ Common.Pages.KMEditor.Routes.parsers KMEditorRoute
                ++ Common.Pages.KnowledgeModels.Routes.parsers KnowledgeModelsRoute
                ++ Common.Pages.UserGroups.Routes.parsers UserGroupsRoute
                ++ Common.Pages.ProjectFiles.Routes.parsers ProjectFilesRoute
                ++ Common.Pages.Projects.Routes.parsers ProjectsRoute
                ++ Common.Pages.Public.Routes.parsers appState PublicRoute
                ++ Common.Pages.Settings.Routes.parsers appState SettingsRoute
                ++ Common.Pages.Users.Routes.parsers appState UsersRoute
                ++ [ map DashboardRoute (s "dashboard")
                   , map KnowledgeModelSecretsRoute (s "knowledge-models" </> s "secrets")
                   , map (PaginationQueryString.wrapRoute1 commentsIndexRoute (Just "updatedAt,desc")) (PaginationQueryString.parser1 (s "comments") (Query.string commentsRouteResolvedFilterId))
                   ]
    in
    oneOf parsers


routeIfAllowed : AppStateLike a -> Route -> Route
routeIfAllowed appState route =
    if isAllowed route appState then
        route

    else
        NotAllowedRoute


isAllowed : Route -> AppStateLike a -> Bool
isAllowed route appState =
    case route of
        CommentsRoute _ _ ->
            True

        DevRoute _ ->
            Common.Pages.Dev.Routes.isAllowed appState

        DashboardRoute ->
            True

        DocumentsRoute _ ->
            Common.Pages.Documents.Routes.isAllowed appState

        DocumentTemplateEditorsRoute dtEditorRoute ->
            Common.Pages.DocumentTemplateEditors.Routes.isAllowed appState dtEditorRoute

        DocumentTemplatesRoute templatesRoute ->
            Common.Pages.DocumentTemplates.Routes.isAllowed templatesRoute appState

        KMEditorRoute kmEditorRoute ->
            Common.Pages.KMEditor.Routes.isAllowed kmEditorRoute appState

        KnowledgeModelsRoute kmPackagesRoute ->
            Common.Pages.KnowledgeModels.Routes.isAllowed kmPackagesRoute appState

        KnowledgeModelSecretsRoute ->
            Feature.knowledgeModelSecrets appState

        UserGroupsRoute userGroupsRoute ->
            Common.Pages.UserGroups.Routes.isAllowed appState userGroupsRoute

        ProjectFilesRoute _ ->
            Common.Pages.ProjectFiles.Routes.isAllowed appState

        ProjectsRoute projectsRoute ->
            Common.Pages.Projects.Routes.isAllowed projectsRoute appState

        PublicRoute _ ->
            True

        SettingsRoute settingsRoute ->
            Common.Pages.Settings.Routes.isAllowed settingsRoute appState

        TenantsRoute _ ->
            Common.Pages.Tenants.Routes.isAllowed appState

        UsersRoute usersRoute ->
            Common.Pages.Users.Routes.isAllowed usersRoute appState

        NotFoundRoute ->
            True

        _ ->
            False


{-| The URL of a route, as the application addresses it (`appState.toUrl`):
the shared pages do not know how an application lays out its addresses, only
each route's own path (`toPath`).
-}
toUrl : { a | toUrl : Route -> String } -> Route -> String
toUrl context route =
    context.toUrl route


{-| The URL of a page about something that names the workspace it belongs to
(`Nothing`: none in particular) — a row of a list that gathers things from more
than one, which links to each where it is. How a workspace is addressed is the
application's.
-}
toUrlIn : { a | toUrlIn : Maybe Uuid -> Route -> String } -> Maybe Uuid -> Route -> String
toUrlIn context mbWorkspaceUuid route =
    context.toUrlIn mbWorkspaceUuid route


{-| Whether the page can be reached from where the reader is, for a link or an
action that leads to it: one that cannot is left out rather than drawn to lead
nowhere. Which pages those are is the application's to say.
-}
isReachable : { a | reachable : Route -> Bool } -> Route -> Bool
isReachable context route =
    context.reachable route


{-| The route's own path, before whatever an application puts in front of it.
-}
toPath : Route -> String
toPath route =
    let
        parts =
            case route of
                CommentsRoute pqs mbResolved ->
                    let
                        params =
                            PaginationQueryString.filterParams
                                [ ( commentsRouteResolvedFilterId, mbResolved ) ]
                    in
                    [ "comments", PaginationQueryString.toUrlWith params pqs ]

                DevRoute adminRoute ->
                    Common.Pages.Dev.Routes.toUrl adminRoute

                DashboardRoute ->
                    [ "dashboard" ]

                DocumentsRoute documentsRoute ->
                    Common.Pages.Documents.Routes.toUrl documentsRoute

                DocumentTemplateEditorsRoute templatesRoute ->
                    Common.Pages.DocumentTemplateEditors.Routes.toUrl templatesRoute

                DocumentTemplatesRoute templatesRoute ->
                    Common.Pages.DocumentTemplates.Routes.toUrl templatesRoute

                KMEditorRoute kmEditorRoute ->
                    Common.Pages.KMEditor.Routes.toUrl kmEditorRoute

                KnowledgeModelsRoute kmPackagesRoute ->
                    Common.Pages.KnowledgeModels.Routes.toUrl kmPackagesRoute

                KnowledgeModelSecretsRoute ->
                    [ "knowledge-models", "secrets" ]

                UserGroupsRoute userGroupsRoute ->
                    Common.Pages.UserGroups.Routes.toUrl userGroupsRoute

                ProjectFilesRoute projectFilesRoute ->
                    Common.Pages.ProjectFiles.Routes.toUrl projectFilesRoute

                ProjectsRoute projectsRoute ->
                    Common.Pages.Projects.Routes.toUrl projectsRoute

                PublicRoute publicRoute ->
                    Common.Pages.Public.Routes.toUrl publicRoute

                SettingsRoute settingsRoute ->
                    Common.Pages.Settings.Routes.toUrl settingsRoute

                TenantsRoute tenantsRoute ->
                    Common.Pages.Tenants.Routes.toUrl tenantsRoute

                UsersRoute usersRoute ->
                    Common.Pages.Users.Routes.toUrl usersRoute

                _ ->
                    []
    in
    "/"
        ++ String.join "/" parts
        |> String.split "/?"
        |> String.join "?"


parse : RoutingAppState a -> Url -> Route
parse appState url =
    case Url.Parser.parse (matchers appState) url of
        Just route ->
            route

        Nothing ->
            NotFoundRoute


navigate : { a | key : Key, toUrl : Route -> String } -> Route -> Cmd msg
navigate context =
    pushUrl context.key << toUrl context


{-| Where the user is sent to log in, coming back to `originalUrl` afterwards.
-}
loginUrl : Maybe String -> String
loginUrl mbOriginalUrl =
    toPath (publicLogin mbOriginalUrl)


cmdNavigateToLogin : { a | key : Key } -> Maybe String -> Cmd msg
cmdNavigateToLogin appState mbOriginalUrl =
    pushUrl appState.key (loginUrl mbOriginalUrl)
