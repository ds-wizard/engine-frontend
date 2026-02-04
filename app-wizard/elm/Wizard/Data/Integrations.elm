port module Wizard.Data.Integrations exposing
    ( ImporterData
    , IntegrationConfig
    , IntegrationWidgetData
    , importerSub
    , integrationWidgetSub
    , openImporter
    , openIntegrationWidget
    )

import Common.Utils.Theme as Theme exposing (Theme)
import Json.Decode as D
import Json.Encode as E
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
