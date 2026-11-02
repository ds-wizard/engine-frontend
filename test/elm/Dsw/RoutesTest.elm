module Dsw.RoutesTest exposing (roundTripTests)

import Common.Api.Models.BootstrapConfig as BootstrapConfig exposing (BootstrapConfig)
import Common.Data.LocaleKind exposing (LocaleKind(..))
import Common.Data.PaginationQueryFilters.FilterOperator exposing (FilterOperator(..))
import Common.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString, SortDirection(..))
import Common.Data.Session as Session exposing (Session)
import Common.Data.UuidOrCurrent as UuidOrCurrent
import Common.Pages.Dev.Routes
import Common.Pages.DocumentTemplateEditors.Editor.Routes as DTEditorRoute
import Common.Pages.DocumentTemplateEditors.Routes
import Common.Pages.DocumentTemplates.Detail.Routes as DocumentTemplateDetailRoute
import Common.Pages.DocumentTemplates.Routes
import Common.Pages.Documents.Routes
import Common.Pages.KMEditor.Editor.Routes as KMEditorRoute
import Common.Pages.KMEditor.Routes
import Common.Pages.KnowledgeModels.Detail.Routes as KnowledgeModelDetailRoute
import Common.Pages.KnowledgeModels.Routes
import Common.Pages.ProjectFiles.Routes
import Common.Pages.Projects.Detail.Routes as ProjectDetailRoute
import Common.Pages.Projects.Routes
import Common.Pages.Public.Routes
import Common.Pages.Settings.Routes exposing (LocalesPage(..))
import Common.Pages.Tenants.Routes
import Common.Pages.Users.Edit.Routes as UserEditRoute
import Common.Pages.Users.Routes
import Common.Plugins.PluginMetadata exposing (PluginMetadata)
import Common.Routes as Routes exposing (Route(..))
import Expect
import Test exposing (Test, describe, test)
import Url
import Uuid exposing (Uuid)


type alias FakeAppState =
    { config : BootstrapConfig
    , session : Session
    , pluginMetadata : List PluginMetadata
    , toUrl : Route -> String
    }


appState : FakeAppState
appState =
    { config = BootstrapConfig.default
    , session = Session.init ""
    , pluginMetadata = []
    , toUrl = Routes.toPath
    }


uuid1 : Uuid
uuid1 =
    Uuid.fromUuidString "8a703cfa-450f-421a-8819-875619ccb54d"


uuid2 : Uuid
uuid2 =
    Uuid.fromUuidString "1a2b3c4d-450f-421a-8819-875619ccb54d"


pqs : PaginationQueryString
pqs =
    PaginationQueryString.withSort (Just "name") SortASC PaginationQueryString.empty


pqsPaged : PaginationQueryString
pqsPaged =
    PaginationQueryString.setPage
        (PaginationQueryString.withSort (Just "updatedAt") SortDESC (PaginationQueryString.fromQ "search"))
        3


routes : List ( String, Route )
routes =
    [ ( "dashboard", DashboardRoute )
    , ( "comments", CommentsRoute pqsPaged (Just "true") )
    , ( "knowledge model secrets", KnowledgeModelSecretsRoute )

    -- Dev
    , ( "dev operations", DevRoute (Common.Pages.Dev.Routes.OperationsRoute Nothing) )
    , ( "dev operations, a section", DevRoute (Common.Pages.Dev.Routes.OperationsRoute (Just "Cache & Co")) )
    , ( "dev persistent commands index", DevRoute (Common.Pages.Dev.Routes.PersistentCommandsIndex pqsPaged (Just "FailedPersistentCommandState")) )
    , ( "dev persistent commands detail", DevRoute (Common.Pages.Dev.Routes.PersistentCommandsDetail uuid1) )

    -- Document template editors
    , ( "dt editors index", DocumentTemplateEditorsRoute (Common.Pages.DocumentTemplateEditors.Routes.IndexRoute pqsPaged) )
    , ( "dt editors create", DocumentTemplateEditorsRoute (Common.Pages.DocumentTemplateEditors.Routes.CreateRoute (Just uuid1) (Just True)) )
    , ( "dt editors editor files", DocumentTemplateEditorsRoute (Common.Pages.DocumentTemplateEditors.Routes.EditorRoute uuid1 DTEditorRoute.Files) )
    , ( "dt editors editor settings", DocumentTemplateEditorsRoute (Common.Pages.DocumentTemplateEditors.Routes.EditorRoute uuid1 (DTEditorRoute.Settings DTEditorRoute.General)) )
    , ( "dt editors editor settings knowledge models", DocumentTemplateEditorsRoute (Common.Pages.DocumentTemplateEditors.Routes.EditorRoute uuid1 (DTEditorRoute.Settings DTEditorRoute.KnowledgeModels)) )
    , ( "dt editors editor settings formats", DocumentTemplateEditorsRoute (Common.Pages.DocumentTemplateEditors.Routes.EditorRoute uuid1 (DTEditorRoute.Settings DTEditorRoute.Formats)) )

    -- Document templates
    , ( "document templates index", DocumentTemplatesRoute (Common.Pages.DocumentTemplates.Routes.IndexRoute pqs) )
    , ( "document templates detail locales", DocumentTemplatesRoute (Common.Pages.DocumentTemplates.Routes.DetailRoute uuid1 DocumentTemplateDetailRoute.Locales) )
    , ( "document templates import", DocumentTemplatesRoute (Common.Pages.DocumentTemplates.Routes.ImportRoute (Just "org:template:1.0.0")) )

    -- Documents
    , ( "documents index", DocumentsRoute (Common.Pages.Documents.Routes.IndexRoute Nothing pqsPaged) )
    , ( "documents index with project", DocumentsRoute (Common.Pages.Documents.Routes.IndexRoute (Just uuid1) pqs) )

    -- KM editor
    , ( "km editor index", KMEditorRoute (Common.Pages.KMEditor.Routes.IndexRoute pqsPaged) )
    , ( "km editor create", KMEditorRoute (Common.Pages.KMEditor.Routes.CreateRoute (Just uuid1) (Just False)) )
    , ( "km editor editor", KMEditorRoute (Common.Pages.KMEditor.Routes.EditorRoute uuid1 (KMEditorRoute.Edit (Just uuid2))) )
    , ( "km editor editor phases", KMEditorRoute (Common.Pages.KMEditor.Routes.EditorRoute uuid1 KMEditorRoute.Phases) )
    , ( "km editor publish", KMEditorRoute (Common.Pages.KMEditor.Routes.PublishRoute uuid1) )
    , ( "km editor migration", KMEditorRoute (Common.Pages.KMEditor.Routes.MigrationRoute uuid1) )

    -- Knowledge models
    , ( "knowledge models index", KnowledgeModelsRoute (Common.Pages.KnowledgeModels.Routes.IndexRoute pqsPaged) )
    , ( "knowledge models detail readme", KnowledgeModelsRoute (Common.Pages.KnowledgeModels.Routes.DetailRoute uuid1 KnowledgeModelDetailRoute.Readme) )
    , ( "knowledge models detail locales", KnowledgeModelsRoute (Common.Pages.KnowledgeModels.Routes.DetailRoute uuid1 KnowledgeModelDetailRoute.Locales) )
    , ( "knowledge models preview", KnowledgeModelsRoute (Common.Pages.KnowledgeModels.Routes.PreviewRoute uuid1 (Just "question-uuid")) )
    , ( "knowledge models resource page", KnowledgeModelsRoute (Common.Pages.KnowledgeModels.Routes.ResourcePageRoute uuid1 "page-uuid") )

    -- Translations, in the settings
    , ( "application translations", SettingsRoute (Common.Pages.Settings.Routes.LocalesRoute UiLocales LocalesListPage) )
    , ( "application translations upload", SettingsRoute (Common.Pages.Settings.Routes.LocalesRoute UiLocales LocalesUploadPage) )
    , ( "application translations registry", SettingsRoute (Common.Pages.Settings.Routes.LocalesRoute UiLocales LocalesRegistryPage) )
    , ( "email translations", SettingsRoute (Common.Pages.Settings.Routes.LocalesRoute MailTemplateLocales LocalesListPage) )
    , ( "email translations upload", SettingsRoute (Common.Pages.Settings.Routes.LocalesRoute MailTemplateLocales LocalesUploadPage) )

    -- Project files
    , ( "project files index", ProjectFilesRoute (Common.Pages.ProjectFiles.Routes.IndexRoute pqs) )
    , ( "project files index paged", ProjectFilesRoute (Common.Pages.ProjectFiles.Routes.IndexRoute pqsPaged) )

    -- Projects
    , ( "projects index", ProjectsRoute (Common.Pages.Projects.Routes.IndexRoute pqs Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) )
    , ( "projects index with filters"
      , ProjectsRoute
            (Common.Pages.Projects.Routes.IndexRoute pqsPaged
                (Just "false")
                (Just (Uuid.toString uuid1))
                (Just OR)
                (Just (Uuid.toString uuid2))
                (Just AND)
                (Just "tag1,tag2")
                (Just AND)
                (Just "org:km:1.0.0")
                (Just OR)
            )
      )
    , ( "projects create", ProjectsRoute (Common.Pages.Projects.Routes.CreateRoute (Just uuid1) Nothing) )
    , ( "projects detail questionnaire", ProjectsRoute (Common.Pages.Projects.Routes.DetailRoute uuid1 (ProjectDetailRoute.Questionnaire (Just "path.to.question") (Just uuid2))) )
    , ( "projects detail documents", ProjectsRoute (Common.Pages.Projects.Routes.DetailRoute uuid1 (ProjectDetailRoute.Documents pqsPaged)) )
    , ( "projects detail files", ProjectsRoute (Common.Pages.Projects.Routes.DetailRoute uuid1 (ProjectDetailRoute.Files pqs)) )
    , ( "projects detail settings", ProjectsRoute (Common.Pages.Projects.Routes.DetailRoute uuid1 ProjectDetailRoute.Settings) )
    , ( "projects create migration", ProjectsRoute (Common.Pages.Projects.Routes.CreateMigrationRoute uuid1) )
    , ( "projects document download", ProjectsRoute (Common.Pages.Projects.Routes.DocumentDownloadRoute uuid1 uuid2) )

    -- Public
    , ( "public login", PublicRoute (Common.Pages.Public.Routes.LoginRoute Nothing) )
    , ( "public login with original url", PublicRoute (Common.Pages.Public.Routes.LoginRoute (Just "/projects")) )
    , ( "public signup", PublicRoute Common.Pages.Public.Routes.SignupRoute )
    , ( "public forgotten password", PublicRoute Common.Pages.Public.Routes.ForgottenPasswordRoute )
    , ( "public logout successful", PublicRoute Common.Pages.Public.Routes.LogoutSuccessful )

    -- Settings
    , ( "settings users", SettingsRoute Common.Pages.Settings.Routes.UsersRoute )
    , ( "settings dashboard and menu", SettingsRoute Common.Pages.Settings.Routes.DashboardAndMenuRoute )
    , ( "settings open id detail", SettingsRoute (Common.Pages.Settings.Routes.OpenIdDetailRoute uuid1) )
    , ( "settings registry", SettingsRoute Common.Pages.Settings.Routes.RegistryRoute )
    , ( "settings usage", SettingsRoute Common.Pages.Settings.Routes.UsageRoute )

    -- Tenants
    , ( "tenants index", TenantsRoute (Common.Pages.Tenants.Routes.IndexRoute pqs Nothing Nothing) )
    , ( "tenants index with filters", TenantsRoute (Common.Pages.Tenants.Routes.IndexRoute pqsPaged (Just "true") (Just "ReadyForUseTenantState")) )
    , ( "tenants detail", TenantsRoute (Common.Pages.Tenants.Routes.DetailRoute uuid1) )

    -- Users
    , ( "users index", UsersRoute (Common.Pages.Users.Routes.IndexRoute pqs Nothing) )
    , ( "users index with role", UsersRoute (Common.Pages.Users.Routes.IndexRoute pqsPaged (Just "admin")) )
    , ( "users create", UsersRoute Common.Pages.Users.Routes.CreateRoute )
    , ( "users edit current", UsersRoute (Common.Pages.Users.Routes.EditRoute UuidOrCurrent.current UserEditRoute.Profile) )
    , ( "users edit tours", UsersRoute (Common.Pages.Users.Routes.EditRoute (UuidOrCurrent.uuid uuid1) UserEditRoute.Tours) )
    , ( "users edit api keys", UsersRoute (Common.Pages.Users.Routes.EditRoute UuidOrCurrent.current UserEditRoute.ApiKeys) )
    ]


roundTripTests : Test
roundTripTests =
    describe "Common.Routes parse (toUrl route) == route"
        (List.map roundTripTest routes)


roundTripTest : ( String, Route ) -> Test
roundTripTest ( name, route ) =
    test name <|
        \_ ->
            let
                url =
                    Routes.toUrl appState route
            in
            case Url.fromString ("http://localhost" ++ url) of
                Just parsedUrl ->
                    Expect.equal route (Routes.parse appState parsedUrl)

                Nothing ->
                    Expect.fail ("Invalid URL: " ++ url)
