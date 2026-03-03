port module Wizard.Data.Integrations exposing
    ( IntegrationConfig
    , IntegrationWidgetData
    , integrationWidgetSub
    , openIntegrationWidget
    )

import Common.Utils.Theme as Theme exposing (Theme)
import Json.Decode as D
import Json.Encode as E
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
