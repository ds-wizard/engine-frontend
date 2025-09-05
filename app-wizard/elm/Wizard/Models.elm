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

import Common.Components.AIAssistant as AIAssistant
import Common.Data.PaginationQueryString as PaginationQueryString
import Random exposing (Seed)
import Wizard.Api.Models.BootstrapConfig as BootstrapConfig
import Wizard.Components.Menu.Models
import Wizard.Data.AppState exposing (AppState)
import Wizard.Data.Session as Session exposing (Session)
import Wizard.Pages.Comments.Models
import Wizard.Pages.Dashboard.Models
import Wizard.Pages.Dev.Models
import Wizard.Pages.DocumentTemplateEditors.Models
import Wizard.Pages.DocumentTemplates.Models
import Wizard.Pages.Documents.Models
import Wizard.Pages.KMEditor.Models
import Wizard.Pages.KnowledgeModelSecrets.Models
import Wizard.Pages.KnowledgeModels.Models
import Wizard.Pages.Locales.Models
import Wizard.Pages.ProjectActions.Models
import Wizard.Pages.ProjectFiles.Models
import Wizard.Pages.ProjectImporters.Models
import Wizard.Pages.Projects.Models
import Wizard.Pages.Public.Models
import Wizard.Pages.Registry.Models
import Wizard.Pages.Settings.Models
import Wizard.Pages.Tenants.Models
import Wizard.Pages.Users.Models
import Wizard.Routes as Routes


type alias Model =
    { appState : AppState
    , menuModel : Wizard.Components.Menu.Models.Model
    , aiAssistantState : AIAssistant.State
    , adminModel : Wizard.Pages.Dev.Models.Model
    , commentsModel : Wizard.Pages.Comments.Models.Model
    , dashboardModel : Wizard.Pages.Dashboard.Models.Model
    , documentsModel : Wizard.Pages.Documents.Models.Model
    , documentTemplateEditorsModel : Wizard.Pages.DocumentTemplateEditors.Models.Model
    , documentTemplatesModel : Wizard.Pages.DocumentTemplates.Models.Model
    , kmEditorModel : Wizard.Pages.KMEditor.Models.Model
    , kmPackagesModel : Wizard.Pages.KnowledgeModels.Models.Model
    , kmSecretsModel : Wizard.Pages.KnowledgeModelSecrets.Models.Model
    , localeModel : Wizard.Pages.Locales.Models.Model
    , projectActionsModel : Wizard.Pages.ProjectActions.Models.Model
    , projectFilesModel : Wizard.Pages.ProjectFiles.Models.Model
    , projectImportersModel : Wizard.Pages.ProjectImporters.Models.Model
    , projectsModel : Wizard.Pages.Projects.Models.Model
    , publicModel : Wizard.Pages.Public.Models.Model
    , registryModel : Wizard.Pages.Registry.Models.Model
    , settingsModel : Wizard.Pages.Settings.Models.Model
    , tenantsModel : Wizard.Pages.Tenants.Models.Model
    , users : Wizard.Pages.Users.Models.Model
    }


initialModel : AppState -> Model
initialModel appState =
    { appState = appState
    , menuModel = Wizard.Components.Menu.Models.initialModel
    , aiAssistantState = AIAssistant.initialState
    , adminModel = Wizard.Pages.Dev.Models.initialModel
    , commentsModel = Wizard.Pages.Comments.Models.initialModel PaginationQueryString.empty Nothing
    , tenantsModel = Wizard.Pages.Tenants.Models.initialModel
    , dashboardModel = Wizard.Pages.Dashboard.Models.initialModel appState
    , documentsModel = Wizard.Pages.Documents.Models.initialModel
    , documentTemplateEditorsModel = Wizard.Pages.DocumentTemplateEditors.Models.initialModel appState
    , documentTemplatesModel = Wizard.Pages.DocumentTemplates.Models.initialModel appState
    , kmEditorModel = Wizard.Pages.KMEditor.Models.initialModel appState
    , kmPackagesModel = Wizard.Pages.KnowledgeModels.Models.initialModel appState
    , kmSecretsModel = Wizard.Pages.KnowledgeModelSecrets.Models.initialModel appState
    , localeModel = Wizard.Pages.Locales.Models.initialModel appState
    , projectActionsModel = Wizard.Pages.ProjectActions.Models.initialModel
    , projectFilesModel = Wizard.Pages.ProjectFiles.Models.initialModel
    , projectImportersModel = Wizard.Pages.ProjectImporters.Models.initialModel
    , projectsModel = Wizard.Pages.Projects.Models.initialModel appState
    , publicModel = Wizard.Pages.Public.Models.initialModel appState
    , registryModel = Wizard.Pages.Registry.Models.initialModel
    , settingsModel = Wizard.Pages.Settings.Models.initialModel appState
    , users = Wizard.Pages.Users.Models.initialModel appState
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
            { model | commentsModel = Wizard.Pages.Comments.Models.initialModel paginationQueryString mbResolved }

        Routes.DashboardRoute ->
            { model | dashboardModel = Wizard.Pages.Dashboard.Models.initialModel model.appState }

        Routes.DevRoute route ->
            { model | adminModel = Wizard.Pages.Dev.Models.initLocalModel route model.adminModel }

        Routes.TenantsRoute route ->
            { model | tenantsModel = Wizard.Pages.Tenants.Models.initLocalModel route model.tenantsModel }

        Routes.DocumentsRoute route ->
            { model | documentsModel = Wizard.Pages.Documents.Models.initLocalModel route model.documentsModel }

        Routes.DocumentTemplatesRoute route ->
            { model | documentTemplatesModel = Wizard.Pages.DocumentTemplates.Models.initLocalModel route model.appState model.documentTemplatesModel }

        Routes.DocumentTemplateEditorsRoute route ->
            { model | documentTemplateEditorsModel = Wizard.Pages.DocumentTemplateEditors.Models.initLocalModel appState route model.documentTemplateEditorsModel }

        Routes.KMEditorRoute route ->
            let
                ( newSeed, kmEditorModel ) =
                    Wizard.Pages.KMEditor.Models.initLocalModel model.appState route model.kmEditorModel
            in
            setSeed newSeed
                { model
                    | kmEditorModel = kmEditorModel
                }

        Routes.KnowledgeModelsRoute route ->
            { model | kmPackagesModel = Wizard.Pages.KnowledgeModels.Models.initLocalModel route model.appState model.kmPackagesModel }

        Routes.KnowledgeModelSecretsRoute ->
            { model | kmSecretsModel = Wizard.Pages.KnowledgeModelSecrets.Models.initialModel appState }

        Routes.LocalesRoute route ->
            { model | localeModel = Wizard.Pages.Locales.Models.initLocalModel model.appState route model.localeModel }

        Routes.ProjectActionsRoute route ->
            { model | projectActionsModel = Wizard.Pages.ProjectActions.Models.initLocalModel route model.projectActionsModel }

        Routes.ProjectFilesRoute route ->
            { model | projectFilesModel = Wizard.Pages.ProjectFiles.Models.initLocalModel route model.projectFilesModel }

        Routes.ProjectImportersRoute route ->
            { model | projectImportersModel = Wizard.Pages.ProjectImporters.Models.initLocalModel route model.projectImportersModel }

        Routes.ProjectsRoute route ->
            { model | projectsModel = Wizard.Pages.Projects.Models.initLocalModel model.appState route model.projectsModel }

        Routes.PublicRoute route ->
            { model | publicModel = Wizard.Pages.Public.Models.initLocalModel model.appState route model.publicModel }

        Routes.RegistryRoute _ ->
            { model | registryModel = Wizard.Pages.Registry.Models.initLocalModel model.registryModel }

        Routes.SettingsRoute route ->
            { model | settingsModel = Wizard.Pages.Settings.Models.initLocalModel model.appState route model.settingsModel }

        Routes.UsersRoute route ->
            { model | users = Wizard.Pages.Users.Models.initLocalModel model.appState route model.users }

        _ ->
            model


userLoggedIn : Model -> Bool
userLoggedIn model =
    Session.exists model.appState.session
