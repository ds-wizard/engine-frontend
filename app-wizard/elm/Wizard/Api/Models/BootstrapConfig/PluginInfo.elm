module Wizard.Api.Models.BootstrapConfig.PluginInfo exposing (PluginInfo, decoder, toTuple)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)


type alias PluginInfo =
    { enabled : Bool
    , url : String
    , uuid : Uuid
    }


decoder : Decoder PluginInfo
decoder =
    D.succeed PluginInfo
        |> D.required "enabled" D.bool
        |> D.required "url" D.string
        |> D.required "uuid" Uuid.decoder


toTuple : PluginInfo -> ( String, Bool )
toTuple pluginInfo =
    ( Uuid.toString pluginInfo.uuid, pluginInfo.enabled )
