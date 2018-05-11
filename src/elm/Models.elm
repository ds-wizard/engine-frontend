module Models exposing (..)

import Auth.Models as AuthModels exposing (JwtToken, Session, sessionDecoder, sessionExists)
import DSPlanner.Models
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (decode, required)
import KMEditor.Create.Models
import KMEditor.Editor.Models
import KMEditor.Index.Models
import KMEditor.Migration.Models
import KMEditor.Publish.Models
import KMPackages.Detail.Models
import KMPackages.Import.Models
import KMPackages.Index.Models
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
    , organizationModel : Organization.Models.Model
    , kmPackagesIndexModel : KMPackages.Index.Models.Model
    , kmPackagesDetailModel : KMPackages.Detail.Models.Model
    , kmPackagesImportModel : KMPackages.Import.Models.Model
    , kmEditorIndexModel : KMEditor.Index.Models.Model
    , kmEditorCreateModel : KMEditor.Create.Models.Model
    , kmEditorPublishModel : KMEditor.Publish.Models.Model
    , kmEditorEditorModel : KMEditor.Editor.Models.Model
    , kmEditorMigrationModel : KMEditor.Migration.Models.Model
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
    , organizationModel = Organization.Models.initialModel
    , kmPackagesIndexModel = KMPackages.Index.Models.initialModel
    , kmPackagesDetailModel = KMPackages.Detail.Models.initialModel
    , kmPackagesImportModel = KMPackages.Import.Models.initialModel
    , kmEditorIndexModel = KMEditor.Index.Models.initialModel
    , kmEditorCreateModel = KMEditor.Create.Models.initialModel
    , kmEditorPublishModel = KMEditor.Publish.Models.initialModel
    , kmEditorEditorModel = KMEditor.Editor.Models.initialModel ""
    , kmEditorMigrationModel = KMEditor.Migration.Models.initialModel ""
    , dsPlannerModel = DSPlanner.Models.initialModel
    , publicModel = Public.Models.initialModel
    , users = Users.Models.initialModel
    }


initLocalModel : Model -> Model
initLocalModel model =
    case model.route of
        Organization ->
            { model | organizationModel = Organization.Models.initialModel }

        KMPackages ->
            { model | kmPackagesIndexModel = KMPackages.Index.Models.initialModel }

        KMPackagesDetail organizationId kmId ->
            { model | kmPackagesDetailModel = KMPackages.Detail.Models.initialModel }

        KMPackagesImport ->
            { model | kmPackagesImportModel = KMPackages.Import.Models.initialModel }

        KMEditor ->
            { model | kmEditorIndexModel = KMEditor.Index.Models.initialModel }

        KMEditorCreate ->
            { model | kmEditorCreateModel = KMEditor.Create.Models.initialModel }

        KMEditorPublish uuid ->
            { model | kmEditorPublishModel = KMEditor.Publish.Models.initialModel }

        KMEditorEditor uuid ->
            { model | kmEditorEditorModel = KMEditor.Editor.Models.initialModel uuid }

        KMEditorMigration uuid ->
            { model | kmEditorMigrationModel = KMEditor.Migration.Models.initialModel uuid }

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
