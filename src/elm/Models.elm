module Models exposing (..)

import Auth.Models as AuthModels exposing (JwtToken, Session, sessionDecoder, sessionExists)
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (decode, required)
import KnowledgeModels.Create.Models
import KnowledgeModels.Index.Models
import KnowledgeModels.Publish.Models
import Organization.Models
import PackageManagement.Detail.Models
import PackageManagement.Import.Models
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
    , packageManagementImportModel : PackageManagement.Import.Models.Model
    , knowledgeModelsIndexModel : KnowledgeModels.Index.Models.Model
    , knowledgeModelsCreateModel : KnowledgeModels.Create.Models.Model
    , knowledgeModelsPublishModel : KnowledgeModels.Publish.Models.Model
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
    , packageManagementImportModel = PackageManagement.Import.Models.initialModel
    , knowledgeModelsIndexModel = KnowledgeModels.Index.Models.initialModel
    , knowledgeModelsCreateModel = KnowledgeModels.Create.Models.initialModel
    , knowledgeModelsPublishModel = KnowledgeModels.Publish.Models.initialModel
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
