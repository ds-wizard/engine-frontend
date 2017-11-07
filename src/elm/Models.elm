module Models exposing (..)

import Auth.Models as AuthModels exposing (JwtToken, Session, sessionDecoder, sessionExists)
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (decode, required)
import Organization.Models
import PackageManagement.Detail.Models
import PackageManagement.Index.Models
import Random.Pcg exposing (Seed, initialSeed)
import Routing exposing (Route)
import UserManagement.Create.Models
import UserManagement.Delete.Models
import UserManagement.Edit.Models
import UserManagement.Index.Models


type alias Model =
    { route : Route
    , seed : Seed
    , authModel : AuthModels.Model
    , session : Session
    , jwt : Maybe JwtToken
    , userManagementIndexModel : UserManagement.Index.Models.Model
    , userManagementCreateModel : UserManagement.Create.Models.Model
    , userManagementEditModel : UserManagement.Edit.Models.Model
    , userManagementDeleteModel : UserManagement.Delete.Models.Model
    , organizationModel : Organization.Models.Model
    , packageManagementIndexModel : PackageManagement.Index.Models.Model
    , packageManagementDetailModel : PackageManagement.Detail.Models.Model
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
    , userManagementEditModel = UserManagement.Edit.Models.initialModel ""
    , userManagementDeleteModel = UserManagement.Delete.Models.initialModel
    , organizationModel = Organization.Models.initialModel
    , packageManagementIndexModel = PackageManagement.Index.Models.initialModel
    , packageManagementDetailModel = PackageManagement.Detail.Models.initialModel
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
