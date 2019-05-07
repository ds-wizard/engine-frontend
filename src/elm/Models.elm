module Models exposing
    ( Flags
    , Model
    , flagsDecoder
    , initLocalModel
    , initialModel
    , setJwt
    , setRoute
    , setSeed
    , setSession
    , userLoggedIn
    )

import Auth.Models exposing (JwtToken, Session, sessionDecoder, sessionExists)
import Browser.Navigation exposing (Key)
import Common.AppState exposing (AppState)
import Common.Menu.Models
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (optional, required)
import KMEditor.Models
import KnowledgeModels.Models
import Organization.Models
import Public.Models
import Questionnaires.Models
import Random exposing (Seed, initialSeed)
import Routing exposing (Route(..))
import Users.Models


type alias Model =
    { appState : AppState
    , menuModel : Common.Menu.Models.Model
    , organizationModel : Organization.Models.Model
    , kmEditorModel : KMEditor.Models.Model
    , kmPackagesModel : KnowledgeModels.Models.Model
    , publicModel : Public.Models.Model
    , dsPlannerModel : Questionnaires.Models.Model
    , users : Users.Models.Model
    }


initialModel : Route -> Flags -> Session -> Maybe JwtToken -> Key -> Model
initialModel route flags session jwt key =
    { appState =
        { route = route
        , seed = initialSeed flags.seed
        , session = session
        , jwt = jwt
        , key = key
        , apiUrl = flags.apiUrl
        , appTitle = flags.appTitle
        , appTitleShort = flags.appTitleShort
        , welcome =
            { warning = flags.welcomeWarning
            , info = flags.welcomeInfo
            }
        }
    , menuModel = Common.Menu.Models.initialModel
    , organizationModel = Organization.Models.initialModel
    , kmEditorModel = KMEditor.Models.initialModel
    , kmPackagesModel = KnowledgeModels.Models.initialModel
    , dsPlannerModel = Questionnaires.Models.initialModel
    , publicModel = Public.Models.initialModel
    , users = Users.Models.initialModel
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


setRoute : Route -> Model -> Model
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
        Organization ->
            { model | organizationModel = Organization.Models.initialModel }

        KMEditor route ->
            { model | kmEditorModel = KMEditor.Models.initLocalModel route model.kmEditorModel }

        KnowledgeModels route ->
            { model | kmPackagesModel = KnowledgeModels.Models.initLocalModel route model.kmPackagesModel }

        Public route ->
            { model | publicModel = Public.Models.initLocalModel route model.publicModel }

        Questionnaires route ->
            { model | dsPlannerModel = Questionnaires.Models.initLocalModel route model.dsPlannerModel }

        Users route ->
            { model | users = Users.Models.initLocalModel route model.users }

        _ ->
            model


userLoggedIn : Model -> Bool
userLoggedIn model =
    sessionExists model.appState.session


type alias Flags =
    { session : Maybe Session
    , seed : Int
    , apiUrl : String
    , appTitle : String
    , appTitleShort : String
    , welcomeWarning : Maybe String
    , welcomeInfo : Maybe String
    }


flagsDecoder : Decoder Flags
flagsDecoder =
    Decode.succeed Flags
        |> required "session" (Decode.nullable sessionDecoder)
        |> required "seed" Decode.int
        |> required "apiUrl" Decode.string
        |> required "appTitle" Decode.string
        |> required "appTitleShort" Decode.string
        |> required "welcomeWarning" (maybe Decode.string)
        |> required "welcomeInfo" (maybe Decode.string)
