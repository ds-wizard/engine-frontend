module Models exposing (..)

import Auth.Models as AuthModels exposing (JwtToken, Session, sessionDecoder, sessionExists)
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (decode, required)
import Random.Pcg exposing (Seed, initialSeed)
import Routing exposing (Route)
import UserManagement.Create.Models
import UserManagement.Delete.Models
import UserManagement.Index.Models


type alias Model =
    { route : Route
    , seed : Seed
    , authModel : AuthModels.Model
    , session : Session
    , jwt : Maybe JwtToken
    , userManagementIndexModel : UserManagement.Index.Models.Model
    , userManagementCreateModel : UserManagement.Create.Models.Model
    , userManagementDeleteModel : UserManagement.Delete.Models.Model
    }


initialModel : Route -> Int -> Session -> Maybe JwtToken -> Model
initialModel route seed session jwt =
    { route = route
    , seed = initialSeed seed
    , authModel = AuthModels.initialModel
    , session = session
    , jwt = jwt
    , userManagementIndexModel = UserManagement.Index.Models.initialModel
    , userManagementCreateModel = UserManagement.Create.Models.initialModel
    , userManagementDeleteModel = UserManagement.Delete.Models.initialModel
    }


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
