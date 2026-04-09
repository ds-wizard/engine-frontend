module Wizard.Api.Models.Event.EditIntegrationPluginEventData exposing
    ( EditIntegrationPluginEventData
    , decoder
    , encode
    , init
    , squash
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extensions as D
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extensions as E
import Wizard.Api.Models.Event.EventField as EventField exposing (EventField)
import Wizard.Api.Models.KnowledgeModel.Annotation as Annotation exposing (Annotation)


type alias EditIntegrationPluginEventData =
    { annotations : EventField (List Annotation)
    , name : EventField String
    , pluginIntegrationId : EventField String
    , pluginIntegrationSettings : EventField String
    , pluginUuid : EventField String
    }


decoder : Decoder EditIntegrationPluginEventData
decoder =
    D.succeed EditIntegrationPluginEventData
        |> D.required "annotations" (EventField.decoder (D.list Annotation.decoder))
        |> D.required "name" (EventField.decoder D.string)
        |> D.required "pluginIntegrationId" (EventField.decoder D.string)
        |> D.required "pluginIntegrationSettings" (EventField.decoder D.valueAsString)
        |> D.required "pluginUuid" (EventField.decoder D.string)


encode : EditIntegrationPluginEventData -> List ( String, E.Value )
encode data =
    [ ( "integrationType", E.string "PluginIntegration" )
    , ( "annotations", EventField.encode (E.list Annotation.encode) data.annotations )
    , ( "name", EventField.encode E.string data.name )
    , ( "pluginIntegrationId", EventField.encode E.string data.pluginIntegrationId )
    , ( "pluginIntegrationSettings", EventField.encode E.stringToValue data.pluginIntegrationSettings )
    , ( "pluginUuid", EventField.encode E.string data.pluginUuid )
    ]


init : EditIntegrationPluginEventData
init =
    { annotations = EventField.empty
    , name = EventField.empty
    , pluginIntegrationId = EventField.empty
    , pluginIntegrationSettings = EventField.empty
    , pluginUuid = EventField.empty
    }


squash : EditIntegrationPluginEventData -> EditIntegrationPluginEventData -> EditIntegrationPluginEventData
squash oldData newData =
    { annotations = EventField.squash oldData.annotations newData.annotations
    , name = EventField.squash oldData.name newData.name
    , pluginIntegrationId = EventField.squash oldData.pluginIntegrationId newData.pluginIntegrationId
    , pluginIntegrationSettings = EventField.squash oldData.pluginIntegrationSettings newData.pluginIntegrationSettings
    , pluginUuid = EventField.squash oldData.pluginUuid newData.pluginUuid
    }
