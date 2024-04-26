module Shared.Data.BootstrapConfig.SignalBridgeConfig exposing
    ( SignalBridgeConfig
    , decoder
    , default
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias SignalBridgeConfig =
    { webSocketUrl : Maybe String
    }


default : SignalBridgeConfig
default =
    { webSocketUrl = Nothing
    }


decoder : Decoder SignalBridgeConfig
decoder =
    D.succeed SignalBridgeConfig
        |> D.required "webSocketUrl" (D.maybe D.string)
