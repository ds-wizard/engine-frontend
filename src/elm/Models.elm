module Models exposing
    ( Flags
    , Model
    , State
    , flagsDecoder
    , initLocalModel
    , initialModel
    , setJwt
    , setRoute
    , setSeed
    , setSession
    , userLoggedIn
    )

import Auth.Models as AuthModels exposing (JwtToken, Session, sessionDecoder, sessionExists)
import Browser.Navigation exposing (Key)
import Common.Menu.Models
import DSPlanner.Models
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (required)
import KMEditor.Models
import KMPackages.Models
import Organization.Models
import Public.Models
import Random exposing (Seed, initialSeed)
import Routing exposing (Route(..))
import Users.Models


type alias Model =
    { state : State
    , menuModel : Common.Menu.Models.Model
    , organizationModel : Organization.Models.Model
    , kmEditorModel : KMEditor.Models.Model
    , kmPackagesModel : KMPackages.Models.Model
    , publicModel : Public.Models.Model
    , dsPlannerModel : DSPlanner.Models.Model
    , users : Users.Models.Model
    }


type alias State =
    { route : Route
    , seed : Seed
    , session : Session
    , jwt : Maybe JwtToken
    , key : Key
    }


initialModel : Route -> Int -> Session -> Maybe JwtToken -> Key -> Model
initialModel route seed session jwt key =
    { state =
        { route = route
        , seed = initialSeed seed
        , session = session
        , jwt = jwt
        , key = key
        }
    , menuModel = Common.Menu.Models.initialModel
    , organizationModel = Organization.Models.initialModel
    , kmEditorModel = KMEditor.Models.initialModel
    , kmPackagesModel = KMPackages.Models.initialModel
    , dsPlannerModel = DSPlanner.Models.initialModel
    , publicModel = Public.Models.initialModel
    , users = Users.Models.initialModel
    }


setSession : Session -> Model -> Model
setSession session model =
    let
        state =
            model.state

        newState =
            { state | session = session }
    in
    { model | state = newState }


setJwt : Maybe JwtToken -> Model -> Model
setJwt jwt model =
    let
        state =
            model.state

        newState =
            { state | jwt = jwt }
    in
    { model | state = newState }


setRoute : Route -> Model -> Model
setRoute route model =
    let
        state =
            model.state

        newState =
            { state | route = route }
    in
    { model | state = newState }


setSeed : Seed -> Model -> Model
setSeed seed model =
    let
        state =
            model.state

        newState =
            { state | seed = seed }
    in
    { model | state = newState }


initLocalModel : Model -> Model
initLocalModel model =
    case model.state.route of
        Organization ->
            { model | organizationModel = Organization.Models.initialModel }

        KMEditor route ->
            { model | kmEditorModel = KMEditor.Models.initLocalModel route model.kmEditorModel }

        KMPackages route ->
            { model | kmPackagesModel = KMPackages.Models.initLocalModel route model.kmPackagesModel }

        Public route ->
            { model | publicModel = Public.Models.initLocalModel route model.publicModel }

        DSPlanner route ->
            { model | dsPlannerModel = DSPlanner.Models.initLocalModel route model.dsPlannerModel }

        Users route ->
            { model | users = Users.Models.initLocalModel route model.users }

        _ ->
            model


userLoggedIn : Model -> Bool
userLoggedIn model =
    sessionExists model.state.session


type alias Flags =
    { session : Maybe Session
    , seed : Int
    }


flagsDecoder : Decoder Flags
flagsDecoder =
    Decode.succeed Flags
        |> required "session" (Decode.nullable sessionDecoder)
        |> required "seed" Decode.int
