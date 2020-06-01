module Wizard.Models exposing
    ( Model
    , initLocalModel
    , initialModel
    , setJwt
    , setRoute
    , setSeed
    , setSession
    , userLoggedIn
    )

import Random exposing (Seed)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.JwtToken exposing (JwtToken)
import Wizard.Common.Menu.Models
import Wizard.Common.Session as Session exposing (Session)
import Wizard.Dashboard.Models
import Wizard.Documents.Models
import Wizard.KMEditor.Models
import Wizard.KnowledgeModels.Models
import Wizard.Public.Models
import Wizard.Questionnaires.Models
import Wizard.Registry.Models
import Wizard.Routes as Routes
import Wizard.Settings.Models
import Wizard.Users.Models


type alias Model =
    { appState : AppState
    , menuModel : Wizard.Common.Menu.Models.Model
    , dashboardModel : Wizard.Dashboard.Models.Model
    , documentsModel : Wizard.Documents.Models.Model
    , kmEditorModel : Wizard.KMEditor.Models.Model
    , kmPackagesModel : Wizard.KnowledgeModels.Models.Model
    , publicModel : Wizard.Public.Models.Model
    , questionnairesModel : Wizard.Questionnaires.Models.Model
    , registryModel : Wizard.Registry.Models.Model
    , settingsModel : Wizard.Settings.Models.Model
    , users : Wizard.Users.Models.Model
    }


initialModel : AppState -> Model
initialModel appState =
    { appState = appState
    , menuModel = Wizard.Common.Menu.Models.initialModel
    , dashboardModel = Wizard.Dashboard.Models.initialModel
    , documentsModel = Wizard.Documents.Models.initialModel
    , kmEditorModel = Wizard.KMEditor.Models.initialModel
    , kmPackagesModel = Wizard.KnowledgeModels.Models.initialModel appState
    , publicModel = Wizard.Public.Models.initialModel
    , questionnairesModel = Wizard.Questionnaires.Models.initialModel appState
    , registryModel = Wizard.Registry.Models.initialModel
    , settingsModel = Wizard.Settings.Models.initialModel
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


setJwt : Maybe JwtToken -> Model -> Model
setJwt jwt model =
    let
        appState =
            model.appState

        newState =
            { appState | jwt = jwt }
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
        Routes.DocumentsRoute route ->
            { model | documentsModel = Wizard.Documents.Models.initLocalModel route model.documentsModel }

        Routes.KMEditorRoute route ->
            { model | kmEditorModel = Wizard.KMEditor.Models.initLocalModel route model.kmEditorModel }

        Routes.KnowledgeModelsRoute route ->
            { model | kmPackagesModel = Wizard.KnowledgeModels.Models.initLocalModel route model.appState model.kmPackagesModel }

        Routes.PublicRoute route ->
            { model | publicModel = Wizard.Public.Models.initLocalModel route model.publicModel }

        Routes.QuestionnairesRoute route ->
            { model | questionnairesModel = Wizard.Questionnaires.Models.initLocalModel model.appState route model.questionnairesModel }

        Routes.RegistryRoute route ->
            { model | registryModel = Wizard.Registry.Models.initLocalModel route model.registryModel }

        Routes.SettingsRoute route ->
            { model | settingsModel = Wizard.Settings.Models.initLocalModel route model.settingsModel }

        Routes.UsersRoute route ->
            { model | users = Wizard.Users.Models.initLocalModel model.appState route model.users }

        _ ->
            model


userLoggedIn : Model -> Bool
userLoggedIn model =
    Session.exists model.appState.session
