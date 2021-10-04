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
import Wizard.Admin.Models
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Menu.Models
import Wizard.Dashboard.Models
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
    , adminModel : Wizard.Admin.Models.Model
    , dashboardModel : Wizard.Dashboard.Models.Model
    , documentsModel : Wizard.Documents.Models.Model
    , kmEditorModel : Wizard.KMEditor.Models.Model
    , kmPackagesModel : Wizard.KnowledgeModels.Models.Model
    , plansModel : Wizard.Projects.Models.Model
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
    , adminModel = Wizard.Admin.Models.initialModel
    , dashboardModel = Wizard.Dashboard.Models.initialModel
    , documentsModel = Wizard.Documents.Models.initialModel
    , kmEditorModel = Wizard.KMEditor.Models.initialModel
    , kmPackagesModel = Wizard.KnowledgeModels.Models.initialModel appState
    , plansModel = Wizard.Projects.Models.initialModel appState
    , publicModel = Wizard.Public.Models.initialModel appState
    , registryModel = Wizard.Registry.Models.initialModel
    , settingsModel = Wizard.Settings.Models.initialModel
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
        Routes.AdminRoute route ->
            { model | adminModel = Wizard.Admin.Models.initLocalModel route model.adminModel }

        Routes.DocumentsRoute route ->
            { model | documentsModel = Wizard.Documents.Models.initLocalModel route model.documentsModel }

        Routes.KMEditorRoute route ->
            { model | kmEditorModel = Wizard.KMEditor.Models.initLocalModel route model.kmEditorModel }

        Routes.KnowledgeModelsRoute route ->
            { model | kmPackagesModel = Wizard.KnowledgeModels.Models.initLocalModel route model.appState model.kmPackagesModel }

        Routes.ProjectsRoute route ->
            { model | plansModel = Wizard.Projects.Models.initLocalModel model.appState route model.plansModel }

        Routes.PublicRoute route ->
            { model | publicModel = Wizard.Public.Models.initLocalModel model.appState route model.publicModel }

        Routes.RegistryRoute route ->
            { model | registryModel = Wizard.Registry.Models.initLocalModel route model.registryModel }

        Routes.SettingsRoute route ->
            { model | settingsModel = Wizard.Settings.Models.initLocalModel route model.settingsModel }

        Routes.TemplatesRoute route ->
            { model | templatesModel = Wizard.Templates.Models.initLocalModel route model.appState model.templatesModel }

        Routes.UsersRoute route ->
            { model | users = Wizard.Users.Models.initLocalModel model.appState route model.users }

        _ ->
            model


userLoggedIn : Model -> Bool
userLoggedIn model =
    Session.exists model.appState.session
