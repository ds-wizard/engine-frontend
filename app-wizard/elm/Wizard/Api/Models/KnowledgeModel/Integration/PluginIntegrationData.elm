module Wizard.Api.Models.KnowledgeModel.Integration.PluginIntegrationData exposing
    ( PluginIntegrationData
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extensions as D
import Json.Decode.Pipeline as D
import Wizard.Api.Models.KnowledgeModel.Annotation as Annotation exposing (Annotation)


type alias PluginIntegrationData =
    { annotations : List Annotation
    , name : String
    , pluginIntegrationId : String
    , pluginIntegrationSettings : String
    , pluginUuid : String
    , uuid : String
    }


decoder : Decoder PluginIntegrationData
decoder =
    D.succeed PluginIntegrationData
        |> D.required "annotations" (D.list Annotation.decoder)
        |> D.required "name" D.string
        |> D.required "pluginIntegrationId" D.string
        |> D.required "pluginIntegrationSettings" D.valueAsString
        |> D.required "pluginUuid" D.string
        |> D.required "uuid" D.string
