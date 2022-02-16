module Shared.Data.BootstrapConfig.ExperimentalConfig exposing
    ( ExperimentalConfig
    , decoder
    , default
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Data.BootstrapConfig.ExperimentalConfig.OwlConfig as OwlConfig exposing (OwlConfig)


type alias ExperimentalConfig =
    { owl : OwlConfig }


default : ExperimentalConfig
default =
    { owl = OwlConfig.default }



-- JSON


decoder : Decoder ExperimentalConfig
decoder =
    D.succeed ExperimentalConfig
        |> D.required "owl" OwlConfig.decoder
