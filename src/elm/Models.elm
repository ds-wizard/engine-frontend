module Models exposing (..)

import Auth.Models as AuthModels exposing (JwtToken, Session, sessionDecoder, sessionExists)
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (decode, required)
import KnowledgeModels.Create.Models
import KnowledgeModels.Editor.Models
import KnowledgeModels.Index.Models
import KnowledgeModels.Migration.Models
import KnowledgeModels.Publish.Models
import Organization.Models
import PackageManagement.Detail.Models
import PackageManagement.Import.Models
import PackageManagement.Index.Models
import Public.Models
import Random.Pcg exposing (Seed, initialSeed)
import Routing exposing (Route(..))
import UserManagement.Create.Models
import UserManagement.Edit.Models
import UserManagement.Index.Models


type alias Model =
    { route : Route
    , seed : Seed
    , session : Session
    , jwt : Maybe JwtToken
    , userManagementIndexModel : UserManagement.Index.Models.Model
    , userManagementCreateModel : UserManagement.Create.Models.Model
    , userManagementEditModel : UserManagement.Edit.Models.Model
    , organizationModel : Organization.Models.Model
    , packageManagementIndexModel : PackageManagement.Index.Models.Model
    , packageManagementDetailModel : PackageManagement.Detail.Models.Model
    , packageManagementImportModel : PackageManagement.Import.Models.Model
    , knowledgeModelsIndexModel : KnowledgeModels.Index.Models.Model
    , knowledgeModelsCreateModel : KnowledgeModels.Create.Models.Model
    , knowledgeModelsPublishModel : KnowledgeModels.Publish.Models.Model
    , knowledgeModelsEditorModel : KnowledgeModels.Editor.Models.Model
    , knowledgeModelsMigrationModel : KnowledgeModels.Migration.Models.Model
    , publicModel : Public.Models.Model
    }


initialModel : Route -> Int -> Session -> Maybe JwtToken -> Model
initialModel route seed session jwt =
    { route = route
    , seed = initialSeed seed
    , session = session
    , jwt = jwt
    , userManagementIndexModel = UserManagement.Index.Models.initialModel
    , userManagementCreateModel = UserManagement.Create.Models.initialModel
    , userManagementEditModel = UserManagement.Edit.Models.initialModel ""
    , organizationModel = Organization.Models.initialModel
    , packageManagementIndexModel = PackageManagement.Index.Models.initialModel
    , packageManagementDetailModel = PackageManagement.Detail.Models.initialModel
    , packageManagementImportModel = PackageManagement.Import.Models.initialModel
    , knowledgeModelsIndexModel = KnowledgeModels.Index.Models.initialModel
    , knowledgeModelsCreateModel = KnowledgeModels.Create.Models.initialModel
    , knowledgeModelsPublishModel = KnowledgeModels.Publish.Models.initialModel
    , knowledgeModelsEditorModel = KnowledgeModels.Editor.Models.initialModel ""
    , knowledgeModelsMigrationModel = KnowledgeModels.Migration.Models.initialModel ""
    , publicModel = Public.Models.initialModel
    }


initLocalModel : Model -> Model
initLocalModel model =
    case model.route of
        UserManagement ->
            { model | userManagementIndexModel = UserManagement.Index.Models.initialModel }

        UserManagementCreate ->
            { model | userManagementCreateModel = UserManagement.Create.Models.initialModel }

        UserManagementEdit uuid ->
            { model | userManagementEditModel = UserManagement.Edit.Models.initialModel uuid }

        Organization ->
            { model | organizationModel = Organization.Models.initialModel }

        PackageManagement ->
            { model | packageManagementIndexModel = PackageManagement.Index.Models.initialModel }

        PackageManagementDetail groupId artifactId ->
            { model | packageManagementDetailModel = PackageManagement.Detail.Models.initialModel }

        PackageManagementImport ->
            { model | packageManagementImportModel = PackageManagement.Import.Models.initialModel }

        KnowledgeModels ->
            { model | knowledgeModelsIndexModel = KnowledgeModels.Index.Models.initialModel }

        KnowledgeModelsCreate ->
            { model | knowledgeModelsCreateModel = KnowledgeModels.Create.Models.initialModel }

        KnowledgeModelsPublish uuid ->
            { model | knowledgeModelsPublishModel = KnowledgeModels.Publish.Models.initialModel }

        KnowledgeModelsEditor uuid ->
            { model | knowledgeModelsEditorModel = KnowledgeModels.Editor.Models.initialModel uuid }

        KnowledgeModelsMigration uuid ->
            { model | knowledgeModelsMigrationModel = KnowledgeModels.Migration.Models.initialModel uuid }

        Public route ->
            { model | publicModel = Public.Models.initLocalModel route model.publicModel }

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
