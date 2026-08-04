module Wizard.Api.Models.BootstrapConfig.AdminConfig exposing
    ( AdminConfig
    , decoder
    , default
    , isEnabled
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias AdminConfig =
    { enabled : Bool }


default : AdminConfig
default =
    { enabled = False }


decoder : Decoder AdminConfig
decoder =
    D.succeed AdminConfig
        |> D.required "enabled" D.bool


isEnabled : AdminConfig -> Bool
isEnabled =
    .enabled
