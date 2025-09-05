port module Wizard.Data.Integrations exposing
    ( ActionData
    , ActionResult
    , ImporterData
    , IntegrationConfig
    , IntegrationWidgetData
    , actionSub
    , importerSub
    , integrationWidgetSub
    , openAction
    , openImporter
    , openIntegrationWidget
    )

import Common.Utils.Theme as Theme exposing (Theme)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Uuid exposing (Uuid)
import Wizard.Components.Questionnaire.Importer.ImporterEvent as ImporterEvent exposing (ImporterEvent)
import Wizard.Data.IntegrationWidgetValue as IntegrationWidgetValue exposing (IntegrationWidgetValue)


type alias IntegrationConfig a =
    { url : String
    , theme : Theme
    , data : a
    }


encodeIntegrationConfig : (a -> E.Value) -> IntegrationConfig a -> E.Value
encodeIntegrationConfig encodeData openData =
    E.object
        [ ( "url", E.string openData.url )
        , ( "theme", E.string (Theme.toStyleString openData.theme) )
        , ( "data", encodeData openData.data )
        ]



-- Action


type alias ActionData =
    { projectUuid : Uuid
    , userToken : Maybe String
    }


encodeActionData : ActionData -> E.Value
encodeActionData data =
    E.object
        [ ( "projectUuid", Uuid.encode data.projectUuid )
        , ( "userToken", E.maybe E.string data.userToken )
        ]


openAction : IntegrationConfig ActionData -> Cmd msg
openAction =
    openActionPort << encodeIntegrationConfig encodeActionData


port openActionPort : E.Value -> Cmd msg


type alias ActionResult =
    { success : Bool
    , message : String
    }


actionResultDecoder : Decoder ActionResult
actionResultDecoder =
    D.succeed ActionResult
        |> D.required "success" D.bool
        |> D.required "message" D.string


actionSub : (Result D.Error ActionResult -> msg) -> Sub msg
actionSub toMsg =
    gotActionData (toMsg << D.decodeValue actionResultDecoder)


port gotActionData : (E.Value -> msg) -> Sub msg



-- Importer


type alias ImporterData =
    { knowledgeModel : String }


encodeImporterData : ImporterData -> E.Value
encodeImporterData data =
    E.object [ ( "knowledgeModel", E.string data.knowledgeModel ) ]


openImporter : IntegrationConfig ImporterData -> Cmd msg
openImporter =
    openImporterPort << encodeIntegrationConfig encodeImporterData


port openImporterPort : E.Value -> Cmd msg


importerSub : (Result D.Error (List ImporterEvent) -> msg) -> Sub msg
importerSub toMsg =
    gotImporterData (toMsg << D.decodeValue (D.list ImporterEvent.decoder))


port gotImporterData : (E.Value -> msg) -> Sub msg



-- Integration Widget


type alias IntegrationWidgetData =
    { path : String }


encodeIntegrationWidgetData : IntegrationWidgetData -> E.Value
encodeIntegrationWidgetData data =
    E.object [ ( "path", E.string data.path ) ]


openIntegrationWidget : IntegrationConfig IntegrationWidgetData -> Cmd msg
openIntegrationWidget =
    openIntegrationWidgetPort << encodeIntegrationConfig encodeIntegrationWidgetData


port openIntegrationWidgetPort : E.Value -> Cmd msg


integrationWidgetSub : (Result D.Error IntegrationWidgetValue -> msg) -> Sub msg
integrationWidgetSub toMsg =
    gotIntegrationWidgetData (toMsg << D.decodeValue IntegrationWidgetValue.decoder)


port gotIntegrationWidgetData : (E.Value -> msg) -> Sub msg
