module Models exposing (..)

import Auth.Models as AuthModels exposing (JwtToken, Session, sessionDecoder, sessionExists)
import Common.Menu.Models
import DSPlanner.Models
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (decode, required)
import KMEditor.Models
import KMPackages.Models
import Organization.Models
import Public.Models
import Random.Pcg exposing (Seed, initialSeed)
import Routing exposing (Route(..))
import Users.Models


type alias Model =
    { route : Route
    , seed : Seed
    , session : Session
    , jwt : Maybe JwtToken
    , menuModel : Common.Menu.Models.Model
    , organizationModel : Organization.Models.Model
    , kmEditorModel : KMEditor.Models.Model
    , kmPackagesModel : KMPackages.Models.Model
    , publicModel : Public.Models.Model
    , dsPlannerModel : DSPlanner.Models.Model
    , users : Users.Models.Model
    }


initialModel : Route -> Int -> Session -> Maybe JwtToken -> Model
initialModel route seed session jwt =
    { route = route
    , seed = initialSeed seed
    , session = session
    , jwt = jwt
    , menuModel = Common.Menu.Models.initialModel
    , organizationModel = Organization.Models.initialModel
    , kmEditorModel = KMEditor.Models.initialModel
    , kmPackagesModel = KMPackages.Models.initialModel
    , dsPlannerModel = DSPlanner.Models.initialModel
    , publicModel = Public.Models.initialModel
    , users = Users.Models.initialModel
    }


initLocalModel : Model -> Model
initLocalModel model =
    case model.route of
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
    sessionExists model.session


type alias Flags =
    { session : Maybe Session
    , seed : Int
    }


flagsDecoder : Decoder Flags
flagsDecoder =
    decode Flags
        |> required "session" (Decode.nullable sessionDecoder)
        |> required "seed" Decode.int
