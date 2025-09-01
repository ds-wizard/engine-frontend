module Wizard.Api.Models.BootstrapConfig.CloudConfig exposing
    ( CloudConfig
    , decoder
    , default
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias CloudConfig =
    { enabled : Bool }


default : CloudConfig
default =
    { enabled = False }


decoder : Decoder CloudConfig
decoder =
    D.succeed CloudConfig
        |> D.required "enabled" D.bool
