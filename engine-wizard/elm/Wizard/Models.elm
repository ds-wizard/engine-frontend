module Wizard.Models exposing
    ( Model
    , addTour
    , initLocalModel
    , initialModel
    , setRoute
    , setSeed
    , setSession
    , userLoggedIn
    )

import Random exposing (Seed)
import Shared.Auth.Session as Session exposing (Session)
import Shared.Data.PaginationQueryString as PaginationQueryString
import Wizard.Api.Models.BootstrapConfig as BootstrapConfig
import Wizard.Comments.Models
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.AIAssistant as AIAssistant
import Wizard.Common.Menu.Models
import Wizard.Dashboard.Models
import Wizard.Dev.Models
import Wizard.DocumentTemplateEditors.Models
import Wizard.DocumentTemplates.Models
import Wizard.Documents.Models
import Wizard.KMEditor.Models
import Wizard.KnowledgeModels.Models
import Wizard.Locales.Models
import Wizard.ProjectActions.Models
import Wizard.ProjectFiles.Models
import Wizard.ProjectImporters.Models
import Wizard.Projects.Models
import Wizard.Public.Models
import Wizard.Registry.Models
import Wizard.Routes as Routes
import Wizard.Settings.Models
import Wizard.Tenants.Models
import Wizard.Users.Models


type alias Model =
    { appState : AppState
    , menuModel : Wizard.Common.Menu.Models.Model
    , aiAssistantState : AIAssistant.State
    , adminModel : Wizard.Dev.Models.Model
    , commentsModel : Wizard.Comments.Models.Model
    , dashboardModel : Wizard.Dashboard.Models.Model
    , documentsModel : Wizard.Documents.Models.Model
    , documentTemplateEditorsModel : Wizard.DocumentTemplateEditors.Models.Model
    , documentTemplatesModel : Wizard.DocumentTemplates.Models.Model
    , kmEditorModel : Wizard.KMEditor.Models.Model
    , kmPackagesModel : Wizard.KnowledgeModels.Models.Model
    , localeModel : Wizard.Locales.Models.Model
    , projectActionsModel : Wizard.ProjectActions.Models.Model
    , projectFilesModel : Wizard.ProjectFiles.Models.Model
    , projectImportersModel : Wizard.ProjectImporters.Models.Model
    , projectsModel : Wizard.Projects.Models.Model
    , publicModel : Wizard.Public.Models.Model
    , registryModel : Wizard.Registry.Models.Model
    , settingsModel : Wizard.Settings.Models.Model
    , tenantsModel : Wizard.Tenants.Models.Model
    , users : Wizard.Users.Models.Model
    }


initialModel : AppState -> Model
initialModel appState =
    { appState = appState
    , menuModel = Wizard.Common.Menu.Models.initialModel
    , aiAssistantState = AIAssistant.initialState
    , adminModel = Wizard.Dev.Models.initialModel
    , commentsModel = Wizard.Comments.Models.initialModel PaginationQueryString.empty Nothing
    , tenantsModel = Wizard.Tenants.Models.initialModel
    , dashboardModel = Wizard.Dashboard.Models.initialModel appState
    , documentsModel = Wizard.Documents.Models.initialModel
    , documentTemplateEditorsModel = Wizard.DocumentTemplateEditors.Models.initialModel appState
    , documentTemplatesModel = Wizard.DocumentTemplates.Models.initialModel appState
    , kmEditorModel = Wizard.KMEditor.Models.initialModel appState
    , kmPackagesModel = Wizard.KnowledgeModels.Models.initialModel appState
    , localeModel = Wizard.Locales.Models.initialModel appState
    , projectActionsModel = Wizard.ProjectActions.Models.initialModel
    , projectFilesModel = Wizard.ProjectFiles.Models.initialModel
    , projectImportersModel = Wizard.ProjectImporters.Models.initialModel
    , projectsModel = Wizard.Projects.Models.initialModel appState
    , publicModel = Wizard.Public.Models.initialModel appState
    , registryModel = Wizard.Registry.Models.initialModel
    , settingsModel = Wizard.Settings.Models.initialModel appState
    , users = Wizard.Users.Models.initialModel appState
    }


setSession : Session -> Model -> Model
setSession session model =
    let
        appState =
            model.appState

        newState =
            { appState | session = session }
    in
    { model | appState = newState }


setRoute : Routes.Route -> Model -> Model
setRoute route model =
    let
        appState =
            model.appState

        newState =
            { appState | route = route }
    in
    { model | appState = newState }


setSeed : Seed -> Model -> Model
setSeed seed model =
    let
        appState =
            model.appState

        newState =
            { appState | seed = seed }
    in
    { model | appState = newState }


addTour : String -> Model -> Model
addTour tourId model =
    let
        config =
            BootstrapConfig.addTour tourId model.appState.config

        appState =
            model.appState
    in
    { model | appState = { appState | config = config } }


initLocalModel : AppState -> Model -> Model
initLocalModel appState model =
    case model.appState.route of
        Routes.CommentsRoute paginationQueryString mbResolved ->
            { model | commentsModel = Wizard.Comments.Models.initialModel paginationQueryString mbResolved }

        Routes.DashboardRoute ->
            { model | dashboardModel = Wizard.Dashboard.Models.initialModel model.appState }

        Routes.DevRoute route ->
            { model | adminModel = Wizard.Dev.Models.initLocalModel route model.adminModel }

        Routes.TenantsRoute route ->
            { model | tenantsModel = Wizard.Tenants.Models.initLocalModel route model.tenantsModel }

        Routes.DocumentsRoute route ->
            { model | documentsModel = Wizard.Documents.Models.initLocalModel route model.documentsModel }

        Routes.DocumentTemplatesRoute route ->
            { model | documentTemplatesModel = Wizard.DocumentTemplates.Models.initLocalModel route model.appState model.documentTemplatesModel }

        Routes.DocumentTemplateEditorsRoute route ->
            { model | documentTemplateEditorsModel = Wizard.DocumentTemplateEditors.Models.initLocalModel appState route model.documentTemplateEditorsModel }

        Routes.KMEditorRoute route ->
            let
                ( newSeed, kmEditorModel ) =
                    Wizard.KMEditor.Models.initLocalModel model.appState route model.kmEditorModel
            in
            setSeed newSeed
                { model
                    | kmEditorModel = kmEditorModel
                }

        Routes.KnowledgeModelsRoute route ->
            { model | kmPackagesModel = Wizard.KnowledgeModels.Models.initLocalModel route model.appState model.kmPackagesModel }

        Routes.LocalesRoute route ->
            { model | localeModel = Wizard.Locales.Models.initLocalModel model.appState route model.localeModel }

        Routes.ProjectActionsRoute route ->
            { model | projectActionsModel = Wizard.ProjectActions.Models.initLocalModel route model.projectActionsModel }

        Routes.ProjectFilesRoute route ->
            { model | projectFilesModel = Wizard.ProjectFiles.Models.initLocalModel route model.projectFilesModel }

        Routes.ProjectImportersRoute route ->
            { model | projectImportersModel = Wizard.ProjectImporters.Models.initLocalModel route model.projectImportersModel }

        Routes.ProjectsRoute route ->
            { model | projectsModel = Wizard.Projects.Models.initLocalModel model.appState route model.projectsModel }

        Routes.PublicRoute route ->
            { model | publicModel = Wizard.Public.Models.initLocalModel model.appState route model.publicModel }

        Routes.RegistryRoute _ ->
            { model | registryModel = Wizard.Registry.Models.initLocalModel model.registryModel }

        Routes.SettingsRoute route ->
            { model | settingsModel = Wizard.Settings.Models.initLocalModel model.appState route model.settingsModel }

        Routes.UsersRoute route ->
            { model | users = Wizard.Users.Models.initLocalModel model.appState route model.users }

        _ ->
            model


userLoggedIn : Model -> Bool
userLoggedIn model =
    Session.exists model.appState.session
