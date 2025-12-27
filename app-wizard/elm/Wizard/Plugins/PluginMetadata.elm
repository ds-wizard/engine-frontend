module Wizard.Plugins.PluginMetadata exposing
    ( PluginMetadata
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)
import Version exposing (Version)


type alias PluginMetadata =
    { uuid : Uuid
    , name : String
    , version : Version
    , description : String
    , pluginApiVersion : Version
    }


decoder : Decoder PluginMetadata
decoder =
    D.succeed PluginMetadata
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "version" Version.decoder
        |> D.required "description" D.string
        |> D.required "pluginApiVersion" Version.decoder
