module Wizard.Models exposing
    ( Model
    , initLocalModel
    , initialModel
    , setRoute
    , setSeed
    , setSession
    , userLoggedIn
    )

import Random exposing (Seed)
import Shared.Auth.Session as Session exposing (Session)
import Wizard.Apps.Models
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Menu.Models
import Wizard.Dashboard.Models
import Wizard.Dev.Models
import Wizard.Documents.Models
import Wizard.KMEditor.Models
import Wizard.KnowledgeModels.Models
import Wizard.Projects.Models
import Wizard.Public.Models
import Wizard.Registry.Models
import Wizard.Routes as Routes
import Wizard.Settings.Models
import Wizard.Templates.Models
import Wizard.Users.Models


type alias Model =
    { appState : AppState
    , menuModel : Wizard.Common.Menu.Models.Model
    , adminModel : Wizard.Dev.Models.Model
    , appsModel : Wizard.Apps.Models.Model
    , dashboardModel : Wizard.Dashboard.Models.Model
    , documentsModel : Wizard.Documents.Models.Model
    , kmEditorModel : Wizard.KMEditor.Models.Model
    , kmPackagesModel : Wizard.KnowledgeModels.Models.Model
    , projectsModel : Wizard.Projects.Models.Model
    , publicModel : Wizard.Public.Models.Model
    , registryModel : Wizard.Registry.Models.Model
    , settingsModel : Wizard.Settings.Models.Model
    , templatesModel : Wizard.Templates.Models.Model
    , users : Wizard.Users.Models.Model
    }


initialModel : AppState -> Model
initialModel appState =
    { appState = appState
    , menuModel = Wizard.Common.Menu.Models.initialModel
    , adminModel = Wizard.Dev.Models.initialModel
    , appsModel = Wizard.Apps.Models.initialModel
    , dashboardModel = Wizard.Dashboard.Models.initialModel
    , documentsModel = Wizard.Documents.Models.initialModel
    , kmEditorModel = Wizard.KMEditor.Models.initialModel appState
    , kmPackagesModel = Wizard.KnowledgeModels.Models.initialModel appState
    , projectsModel = Wizard.Projects.Models.initialModel appState
    , publicModel = Wizard.Public.Models.initialModel appState
    , registryModel = Wizard.Registry.Models.initialModel
    , settingsModel = Wizard.Settings.Models.initialModel appState
    , templatesModel = Wizard.Templates.Models.initialModel appState
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


initLocalModel : Model -> Model
initLocalModel model =
    case model.appState.route of
        Routes.DevRoute route ->
            { model | adminModel = Wizard.Dev.Models.initLocalModel route model.adminModel }

        Routes.AppsRoute route ->
            { model | appsModel = Wizard.Apps.Models.initLocalModel route model.appsModel }

        Routes.DocumentsRoute route ->
            { model | documentsModel = Wizard.Documents.Models.initLocalModel route model.documentsModel }

        Routes.KMEditorRoute route ->
            { model | kmEditorModel = Wizard.KMEditor.Models.initLocalModel model.appState route model.kmEditorModel }

        Routes.KnowledgeModelsRoute route ->
            { model | kmPackagesModel = Wizard.KnowledgeModels.Models.initLocalModel route model.appState model.kmPackagesModel }

        Routes.ProjectsRoute route ->
            { model | projectsModel = Wizard.Projects.Models.initLocalModel model.appState route model.projectsModel }

        Routes.PublicRoute route ->
            { model | publicModel = Wizard.Public.Models.initLocalModel model.appState route model.publicModel }

        Routes.RegistryRoute route ->
            { model | registryModel = Wizard.Registry.Models.initLocalModel route model.registryModel }

        Routes.SettingsRoute route ->
            { model | settingsModel = Wizard.Settings.Models.initLocalModel model.appState route model.settingsModel }

        Routes.TemplatesRoute route ->
            { model | templatesModel = Wizard.Templates.Models.initLocalModel route model.appState model.templatesModel }

        Routes.UsersRoute route ->
            { model | users = Wizard.Users.Models.initLocalModel model.appState route model.users }

        _ ->
            model


userLoggedIn : Model -> Bool
userLoggedIn model =
    Session.exists model.appState.session
