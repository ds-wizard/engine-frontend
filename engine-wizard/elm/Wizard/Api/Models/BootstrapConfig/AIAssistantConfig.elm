module Wizard.Api.Models.BootstrapConfig.AIAssistantConfig exposing
    ( AIAssistantConfig
    , decoder
    , default
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias AIAssistantConfig =
    { enabled : Bool
    }


default : AIAssistantConfig
default =
    { enabled = False }


decoder : Decoder AIAssistantConfig
decoder =
    D.succeed AIAssistantConfig
        |> D.required "enabled" D.bool
