module Dsw.Pages exposing
    ( Frame(..)
    , Model
    , Msg
    , entityName
    , frame
    , init
    , initialModel
    , isGuarded
    , subscriptions
    , update
    , view
    )

{-| The client's pages: each shared page, mounted once.

`Dsw.elm` hands a route here. This module initializes its page, routes its
messages and subscriptions, draws it and asks it whether leaving is safe, with
the feature-router contract, and says which frame a
page goes in. Every page is mounted as it comes (`RowPolicy.default`): this
client has one place for each.

A route with no page here (Not Found, Not Allowed) initializes nothing and
draws `Nothing`, and the shell draws it itself.

-}

import Common.Data.AppState exposing (AppState)
import Common.Data.RowPolicy as RowPolicy
import Common.Data.UpdateConfig as UpdateConfig exposing (UpdateConfig)
import Common.Pages.Comments as Comments
import Common.Pages.Dashboard as Dashboard
import Common.Pages.Dev as Dev
import Common.Pages.DocumentTemplateEditors as DocumentTemplateEditors
import Common.Pages.DocumentTemplates as DocumentTemplates
import Common.Pages.Documents as Documents
import Common.Pages.KMEditor as KMEditor
import Common.Pages.KnowledgeModelSecrets as KnowledgeModelSecrets
import Common.Pages.KnowledgeModels as KnowledgeModels
import Common.Pages.ProjectFiles as ProjectFiles
import Common.Pages.Projects as Projects
import Common.Pages.Public as Public
import Common.Pages.Settings as Settings
import Common.Pages.Tenants as Tenants
import Common.Pages.UserGroups as UserGroups
import Common.Pages.Users as Users
import Common.Routes as Routes
import Html exposing (Html)
import Random


type alias Model =
    { comments : Comments.Model
    , dashboard : Dashboard.Model
    , dev : Dev.Model
    , documentTemplateEditors : DocumentTemplateEditors.Model
    , documentTemplates : DocumentTemplates.Model
    , documents : Documents.Model
    , kmEditor : KMEditor.Model
    , knowledgeModelSecrets : KnowledgeModelSecrets.Model
    , knowledgeModels : KnowledgeModels.Model
    , userGroups : UserGroups.Model
    , projectFiles : ProjectFiles.Model
    , projects : Projects.Model
    , public : Public.Model
    , settings : Settings.Model
    , tenants : Tenants.Model
    , users : Users.Model
    }


type Msg
    = CommentsMsg Comments.Msg
    | DashboardMsg Dashboard.Msg
    | DevMsg Dev.Msg
    | DocumentTemplateEditorsMsg DocumentTemplateEditors.Msg
    | DocumentTemplatesMsg DocumentTemplates.Msg
    | DocumentsMsg Documents.Msg
    | KMEditorMsg KMEditor.Msg
    | KnowledgeModelSecretsMsg KnowledgeModelSecrets.Msg
    | KnowledgeModelsMsg KnowledgeModels.Msg
    | UserGroupsMsg UserGroups.Msg
    | ProjectFilesMsg ProjectFiles.Msg
    | ProjectsMsg Projects.Msg
    | PublicMsg Public.Msg
    | SettingsMsg Settings.Msg
    | TenantsMsg Tenants.Msg
    | UsersMsg Users.Msg


{-| The frame a page is drawn in: the application's, the public one, or the
application's for a signed-in reader and the public one for anybody else (a
project or a knowledge model that may be public).
-}
type Frame
    = AppFrame
    | MixedFrame
    | PublicFrame


frame : Routes.Route -> Frame
frame route =
    case route of
        Routes.KnowledgeModelsRoute _ ->
            MixedFrame

        Routes.ProjectsRoute _ ->
            MixedFrame

        Routes.PublicRoute _ ->
            PublicFrame

        Routes.NotFoundRoute ->
            MixedFrame

        _ ->
            AppFrame


initialModel : AppState -> Model
initialModel appState =
    { comments = Comments.initialModel appState
    , dashboard = Dashboard.initialModel
    , dev = Dev.initialModel appState
    , documentTemplateEditors = DocumentTemplateEditors.initialModel appState
    , documentTemplates = DocumentTemplates.initialModel appState
    , documents = Documents.initialModel appState
    , kmEditor = KMEditor.initialModel appState
    , knowledgeModelSecrets = KnowledgeModelSecrets.initialModel appState
    , knowledgeModels = KnowledgeModels.initialModel appState
    , userGroups = UserGroups.initialModel
    , projectFiles = ProjectFiles.initialModel appState
    , projects = Projects.initialModel appState
    , public = Public.initialModel appState
    , settings = Settings.initialModel appState
    , tenants = Tenants.initialModel appState
    , users = Users.initialModel appState
    }


init : AppState -> Routes.Route -> Model -> ( Model, Cmd Msg )
init appState sharedRoute model =
    case sharedRoute of
        Routes.CommentsRoute pqs mbResolved ->
            let
                ( pageModel, pageCmd ) =
                    Comments.init appState model.comments pqs mbResolved
            in
            ( { model | comments = pageModel }, Cmd.map CommentsMsg pageCmd )

        Routes.DashboardRoute ->
            let
                ( pageModel, pageCmd ) =
                    Dashboard.init appState model.dashboard
            in
            ( { model | dashboard = pageModel }, Cmd.map DashboardMsg pageCmd )

        Routes.DevRoute route ->
            let
                ( pageModel, pageCmd ) =
                    Dev.init appState model.dev route
            in
            ( { model | dev = pageModel }, Cmd.map DevMsg pageCmd )

        Routes.DocumentTemplateEditorsRoute route ->
            let
                ( pageModel, pageCmd ) =
                    DocumentTemplateEditors.init appState model.documentTemplateEditors route
            in
            ( { model | documentTemplateEditors = pageModel }, Cmd.map DocumentTemplateEditorsMsg pageCmd )

        Routes.DocumentTemplatesRoute route ->
            let
                ( pageModel, pageCmd ) =
                    DocumentTemplates.init appState model.documentTemplates route
            in
            ( { model | documentTemplates = pageModel }, Cmd.map DocumentTemplatesMsg pageCmd )

        Routes.DocumentsRoute route ->
            let
                ( pageModel, pageCmd ) =
                    Documents.init appState model.documents route
            in
            ( { model | documents = pageModel }, Cmd.map DocumentsMsg pageCmd )

        Routes.KMEditorRoute route ->
            let
                ( pageModel, pageCmd ) =
                    KMEditor.init appState model.kmEditor route
            in
            ( { model | kmEditor = pageModel }, Cmd.map KMEditorMsg pageCmd )

        Routes.KnowledgeModelSecretsRoute ->
            let
                ( pageModel, pageCmd ) =
                    KnowledgeModelSecrets.init appState model.knowledgeModelSecrets
            in
            ( { model | knowledgeModelSecrets = pageModel }, Cmd.map KnowledgeModelSecretsMsg pageCmd )

        Routes.KnowledgeModelsRoute route ->
            let
                ( pageModel, pageCmd ) =
                    KnowledgeModels.init appState model.knowledgeModels route
            in
            ( { model | knowledgeModels = pageModel }, Cmd.map KnowledgeModelsMsg pageCmd )

        Routes.UserGroupsRoute route ->
            let
                ( pageModel, pageCmd ) =
                    UserGroups.init appState model.userGroups route
            in
            ( { model | userGroups = pageModel }, Cmd.map UserGroupsMsg pageCmd )

        Routes.ProjectFilesRoute route ->
            let
                ( pageModel, pageCmd ) =
                    ProjectFiles.init appState model.projectFiles route
            in
            ( { model | projectFiles = pageModel }, Cmd.map ProjectFilesMsg pageCmd )

        Routes.ProjectsRoute route ->
            let
                ( pageModel, pageCmd ) =
                    Projects.init appState model.projects route
            in
            ( { model | projects = pageModel }, Cmd.map ProjectsMsg pageCmd )

        Routes.PublicRoute route ->
            let
                ( pageModel, pageCmd ) =
                    Public.init appState model.public route
            in
            ( { model | public = pageModel }, Cmd.map PublicMsg pageCmd )

        Routes.SettingsRoute route ->
            let
                ( pageModel, pageCmd ) =
                    Settings.init appState model.settings route
            in
            ( { model | settings = pageModel }, Cmd.map SettingsMsg pageCmd )

        Routes.UsersRoute route ->
            let
                ( pageModel, pageCmd ) =
                    Users.init appState model.users route
            in
            ( { model | users = pageModel }, Cmd.map UsersMsg pageCmd )

        Routes.TenantsRoute route ->
            let
                ( pageModel, pageCmd ) =
                    Tenants.init appState model.tenants route
            in
            ( { model | tenants = pageModel }, Cmd.map TenantsMsg pageCmd )

        _ ->
            ( model, Cmd.none )


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Random.Seed, Model, Cmd msg )
update cfg appState msg model =
    case msg of
        CommentsMsg pageMsg ->
            let
                ( seed, pageModel, pageCmd ) =
                    Comments.update (UpdateConfig.map CommentsMsg cfg) appState pageMsg model.comments
            in
            ( seed, { model | comments = pageModel }, pageCmd )

        DashboardMsg pageMsg ->
            let
                ( seed, pageModel, pageCmd ) =
                    Dashboard.update (UpdateConfig.map DashboardMsg cfg) appState pageMsg model.dashboard
            in
            ( seed, { model | dashboard = pageModel }, pageCmd )

        DevMsg pageMsg ->
            let
                ( seed, pageModel, pageCmd ) =
                    Dev.update (UpdateConfig.map DevMsg cfg) appState pageMsg model.dev
            in
            ( seed, { model | dev = pageModel }, pageCmd )

        DocumentTemplateEditorsMsg pageMsg ->
            let
                ( seed, pageModel, pageCmd ) =
                    DocumentTemplateEditors.update (UpdateConfig.map DocumentTemplateEditorsMsg cfg) appState pageMsg model.documentTemplateEditors
            in
            ( seed, { model | documentTemplateEditors = pageModel }, pageCmd )

        DocumentTemplatesMsg pageMsg ->
            let
                ( seed, pageModel, pageCmd ) =
                    DocumentTemplates.update (UpdateConfig.map DocumentTemplatesMsg cfg) appState pageMsg model.documentTemplates
            in
            ( seed, { model | documentTemplates = pageModel }, pageCmd )

        DocumentsMsg pageMsg ->
            let
                ( seed, pageModel, pageCmd ) =
                    Documents.update (UpdateConfig.map DocumentsMsg cfg) appState pageMsg model.documents
            in
            ( seed, { model | documents = pageModel }, pageCmd )

        KMEditorMsg pageMsg ->
            let
                ( seed, pageModel, pageCmd ) =
                    KMEditor.update (UpdateConfig.map KMEditorMsg cfg) appState pageMsg model.kmEditor
            in
            ( seed, { model | kmEditor = pageModel }, pageCmd )

        KnowledgeModelSecretsMsg pageMsg ->
            let
                ( seed, pageModel, pageCmd ) =
                    KnowledgeModelSecrets.update (UpdateConfig.map KnowledgeModelSecretsMsg cfg) appState pageMsg model.knowledgeModelSecrets
            in
            ( seed, { model | knowledgeModelSecrets = pageModel }, pageCmd )

        KnowledgeModelsMsg pageMsg ->
            let
                ( seed, pageModel, pageCmd ) =
                    KnowledgeModels.update (UpdateConfig.map KnowledgeModelsMsg cfg) appState pageMsg model.knowledgeModels
            in
            ( seed, { model | knowledgeModels = pageModel }, pageCmd )

        UserGroupsMsg pageMsg ->
            let
                ( seed, pageModel, pageCmd ) =
                    UserGroups.update (UpdateConfig.map UserGroupsMsg cfg) appState pageMsg model.userGroups
            in
            ( seed, { model | userGroups = pageModel }, pageCmd )

        ProjectFilesMsg pageMsg ->
            let
                ( seed, pageModel, pageCmd ) =
                    ProjectFiles.update (UpdateConfig.map ProjectFilesMsg cfg) appState pageMsg model.projectFiles
            in
            ( seed, { model | projectFiles = pageModel }, pageCmd )

        ProjectsMsg pageMsg ->
            let
                ( seed, pageModel, pageCmd ) =
                    Projects.update (UpdateConfig.map ProjectsMsg cfg) appState pageMsg model.projects
            in
            ( seed, { model | projects = pageModel }, pageCmd )

        PublicMsg pageMsg ->
            let
                ( seed, pageModel, pageCmd ) =
                    Public.update (UpdateConfig.map PublicMsg cfg) appState pageMsg model.public
            in
            ( seed, { model | public = pageModel }, pageCmd )

        SettingsMsg pageMsg ->
            let
                ( seed, pageModel, pageCmd ) =
                    Settings.update (UpdateConfig.map SettingsMsg cfg) appState pageMsg model.settings
            in
            ( seed, { model | settings = pageModel }, pageCmd )

        TenantsMsg pageMsg ->
            let
                ( seed, pageModel, pageCmd ) =
                    Tenants.update (UpdateConfig.map TenantsMsg cfg) appState pageMsg model.tenants
            in
            ( seed, { model | tenants = pageModel }, pageCmd )

        UsersMsg pageMsg ->
            let
                ( seed, pageModel, pageCmd ) =
                    Users.update (UpdateConfig.map UsersMsg cfg) appState pageMsg model.users
            in
            ( seed, { model | users = pageModel }, pageCmd )


subscriptions : AppState -> Routes.Route -> Model -> Sub Msg
subscriptions appState sharedRoute model =
    case sharedRoute of
        Routes.CommentsRoute pqs mbResolved ->
            Sub.map CommentsMsg (Comments.subscriptions appState pqs mbResolved model.comments)

        Routes.DashboardRoute ->
            Sub.map DashboardMsg (Dashboard.subscriptions appState model.dashboard)

        Routes.DevRoute route ->
            Sub.map DevMsg (Dev.subscriptions appState route model.dev)

        Routes.DocumentTemplateEditorsRoute route ->
            Sub.map DocumentTemplateEditorsMsg (DocumentTemplateEditors.subscriptions appState route model.documentTemplateEditors)

        Routes.DocumentTemplatesRoute route ->
            Sub.map DocumentTemplatesMsg (DocumentTemplates.subscriptions appState route model.documentTemplates)

        Routes.DocumentsRoute route ->
            Sub.map DocumentsMsg (Documents.subscriptions appState route model.documents)

        Routes.KMEditorRoute route ->
            Sub.map KMEditorMsg (KMEditor.subscriptions appState route model.kmEditor)

        Routes.KnowledgeModelSecretsRoute ->
            Sub.map KnowledgeModelSecretsMsg (KnowledgeModelSecrets.subscriptions appState model.knowledgeModelSecrets)

        Routes.KnowledgeModelsRoute route ->
            Sub.map KnowledgeModelsMsg (KnowledgeModels.subscriptions appState route model.knowledgeModels)

        Routes.UserGroupsRoute route ->
            Sub.map UserGroupsMsg (UserGroups.subscriptions appState route model.userGroups)

        Routes.ProjectFilesRoute route ->
            Sub.map ProjectFilesMsg (ProjectFiles.subscriptions appState route model.projectFiles)

        Routes.ProjectsRoute route ->
            Sub.map ProjectsMsg (Projects.subscriptions appState route model.projects)

        Routes.PublicRoute route ->
            Sub.map PublicMsg (Public.subscriptions appState route model.public)

        Routes.SettingsRoute route ->
            Sub.map SettingsMsg (Settings.subscriptions appState route model.settings)

        Routes.UsersRoute route ->
            Sub.map UsersMsg (Users.subscriptions appState route model.users)

        Routes.TenantsRoute route ->
            Sub.map TenantsMsg (Tenants.subscriptions appState route model.tenants)

        _ ->
            Sub.none


{-| The page for a shared route, or `Nothing` for a route the shell draws itself.
-}
view : AppState -> Routes.Route -> Model -> Maybe (Html Msg)
view appState sharedRoute model =
    case sharedRoute of
        Routes.CommentsRoute pqs mbResolved ->
            Just (Html.map CommentsMsg (Comments.view appState pqs mbResolved model.comments))

        Routes.DashboardRoute ->
            Just (Html.map DashboardMsg (Dashboard.view appState model.dashboard))

        Routes.DevRoute route ->
            Just (Html.map DevMsg (Dev.view appState route model.dev))

        Routes.DocumentTemplateEditorsRoute route ->
            Just (Html.map DocumentTemplateEditorsMsg (DocumentTemplateEditors.view appState route model.documentTemplateEditors))

        Routes.DocumentTemplatesRoute route ->
            Just (Html.map DocumentTemplatesMsg (DocumentTemplates.view RowPolicy.default appState route model.documentTemplates))

        Routes.DocumentsRoute route ->
            Just (Html.map DocumentsMsg (Documents.view appState route model.documents))

        Routes.KMEditorRoute route ->
            Just (Html.map KMEditorMsg (KMEditor.view appState route model.kmEditor))

        Routes.KnowledgeModelSecretsRoute ->
            Just (Html.map KnowledgeModelSecretsMsg (KnowledgeModelSecrets.view RowPolicy.default appState model.knowledgeModelSecrets))

        Routes.KnowledgeModelsRoute route ->
            Just (Html.map KnowledgeModelsMsg (KnowledgeModels.view RowPolicy.default appState route model.knowledgeModels))

        Routes.UserGroupsRoute route ->
            Just (Html.map UserGroupsMsg (UserGroups.view appState route model.userGroups))

        Routes.ProjectFilesRoute route ->
            Just (Html.map ProjectFilesMsg (ProjectFiles.view appState route model.projectFiles))

        Routes.ProjectsRoute route ->
            Just (Html.map ProjectsMsg (Projects.view appState route model.projects))

        Routes.PublicRoute route ->
            Just (Html.map PublicMsg (Public.view appState route model.public))

        Routes.SettingsRoute route ->
            Just (Html.map SettingsMsg (Settings.view (Settings.defaultMount appState) appState route model.settings))

        Routes.UsersRoute route ->
            Just (Html.map UsersMsg (Users.view appState route model.users))

        Routes.TenantsRoute route ->
            Just (Html.map TenantsMsg (Tenants.view appState route model.tenants))

        _ ->
            Nothing


{-| Whether the page being left has unsaved changes. `mbNextRoute` is where the
user is going, for a page that cares.
-}
isGuarded : AppState -> Maybe Routes.Route -> Routes.Route -> Model -> Maybe String
isGuarded appState mbNextRoute sharedRoute model =
    case sharedRoute of
        Routes.CommentsRoute pqs mbResolved ->
            Comments.isGuarded appState pqs mbResolved model.comments

        Routes.DashboardRoute ->
            Dashboard.isGuarded appState model.dashboard

        Routes.DevRoute route ->
            Dev.isGuarded appState route model.dev

        Routes.DocumentTemplateEditorsRoute route ->
            DocumentTemplateEditors.isGuarded appState route mbNextRoute model.documentTemplateEditors

        Routes.DocumentTemplatesRoute route ->
            DocumentTemplates.isGuarded appState route model.documentTemplates

        Routes.DocumentsRoute route ->
            Documents.isGuarded appState route model.documents

        Routes.KMEditorRoute route ->
            KMEditor.isGuarded appState route mbNextRoute model.kmEditor

        Routes.KnowledgeModelSecretsRoute ->
            KnowledgeModelSecrets.isGuarded appState model.knowledgeModelSecrets

        Routes.KnowledgeModelsRoute route ->
            KnowledgeModels.isGuarded appState route model.knowledgeModels

        Routes.UserGroupsRoute route ->
            UserGroups.isGuarded appState route model.userGroups

        Routes.ProjectFilesRoute route ->
            ProjectFiles.isGuarded appState route model.projectFiles

        Routes.ProjectsRoute route ->
            Projects.isGuarded appState route mbNextRoute model.projects

        Routes.PublicRoute route ->
            Public.isGuarded appState route model.public

        Routes.SettingsRoute route ->
            Settings.isGuarded appState route model.settings

        Routes.UsersRoute route ->
            Users.isGuarded appState route model.users

        Routes.TenantsRoute route ->
            Tenants.isGuarded appState route model.tenants

        _ ->
            Nothing


{-| The name of what the page at a route is about, for the window's title: a
detail page's entity, once it has arrived.
-}
entityName : AppState -> Model -> Routes.Route -> Maybe String
entityName appState model sharedRoute =
    case sharedRoute of
        Routes.DocumentTemplateEditorsRoute route ->
            DocumentTemplateEditors.entityName model.documentTemplateEditors route

        Routes.DocumentTemplatesRoute route ->
            DocumentTemplates.entityName model.documentTemplates route

        Routes.KMEditorRoute route ->
            KMEditor.entityName model.kmEditor route

        Routes.KnowledgeModelsRoute route ->
            KnowledgeModels.entityName model.knowledgeModels route

        Routes.UserGroupsRoute route ->
            UserGroups.entityName model.userGroups route

        Routes.ProjectsRoute route ->
            Projects.entityName model.projects route

        Routes.SettingsRoute route ->
            Settings.entityName appState.locale model.settings route

        Routes.UsersRoute route ->
            Users.entityName model.users route

        Routes.TenantsRoute route ->
            Tenants.entityName model.tenants route

        _ ->
            Nothing
