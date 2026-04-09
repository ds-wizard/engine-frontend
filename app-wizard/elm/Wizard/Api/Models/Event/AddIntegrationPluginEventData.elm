module Wizard.Api.Models.Event.AddIntegrationPluginEventData exposing
    ( AddIntegrationPluginEventData
    , decoder
    , encode
    , toIntegration
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extensions as D
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extensions as E
import Wizard.Api.Models.KnowledgeModel.Annotation as Annotation exposing (Annotation)
import Wizard.Api.Models.KnowledgeModel.Integration exposing (Integration(..))


type alias AddIntegrationPluginEventData =
    { annotations : List Annotation
    , name : String
    , pluginIntegrationId : String
    , pluginIntegrationSettings : String
    , pluginUuid : String
    }


decoder : Decoder AddIntegrationPluginEventData
decoder =
    D.succeed AddIntegrationPluginEventData
        |> D.required "annotations" (D.list Annotation.decoder)
        |> D.required "name" D.string
        |> D.required "pluginIntegrationId" D.string
        |> D.required "pluginIntegrationSettings" D.valueAsString
        |> D.required "pluginUuid" D.string


encode : AddIntegrationPluginEventData -> List ( String, E.Value )
encode data =
    [ ( "integrationType", E.string "PluginIntegration" )
    , ( "annotations", E.list Annotation.encode data.annotations )
    , ( "name", E.string data.name )
    , ( "pluginIntegrationId", E.string data.pluginIntegrationId )
    , ( "pluginIntegrationSettings", E.stringToValue data.pluginIntegrationSettings )
    , ( "pluginUuid", E.string data.pluginUuid )
    ]


toIntegration : String -> AddIntegrationPluginEventData -> Integration
toIntegration uuid data =
    PluginIntegration
        { annotations = data.annotations
        , name = data.name
        , pluginIntegrationId = data.pluginIntegrationId
        , pluginIntegrationSettings = data.pluginIntegrationSettings
        , pluginUuid = data.pluginUuid
        , uuid = uuid
        }
