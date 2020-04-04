module Shared.Data.BootstrapConfig.AuthenticationConfig.OpenIDServiceConfig exposing
    ( OpenIDServiceConfig
    , background
    , color
    , decoder
    , icon
    , id
    , name
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type OpenIDServiceConfig
    = OpenIDServiceConfig Internals


type alias Internals =
    { id : String
    , name : String
    , style : Style
    }


type alias Style =
    { background : Maybe String
    , color : Maybe String
    , icon : Maybe String
    }


id : OpenIDServiceConfig -> String
id (OpenIDServiceConfig config) =
    config.id


name : OpenIDServiceConfig -> String
name (OpenIDServiceConfig config) =
    config.id


background : OpenIDServiceConfig -> Maybe String
background (OpenIDServiceConfig config) =
    config.style.background


color : OpenIDServiceConfig -> Maybe String
color (OpenIDServiceConfig config) =
    config.style.color


icon : OpenIDServiceConfig -> Maybe String
icon (OpenIDServiceConfig config) =
    config.style.icon


decoder : Decoder OpenIDServiceConfig
decoder =
    D.succeed Internals
        |> D.required "id" D.string
        |> D.required "name" D.string
        |> D.required "style" styleConfigDecoder
        |> D.map OpenIDServiceConfig


styleConfigDecoder : Decoder Style
styleConfigDecoder =
    D.succeed Style
        |> D.required "background" (D.maybe D.string)
        |> D.required "color" (D.maybe D.string)
        |> D.required "icon" (D.maybe D.string)
