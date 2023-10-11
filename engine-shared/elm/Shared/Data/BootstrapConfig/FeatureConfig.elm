module Shared.Data.BootstrapConfig.FeatureConfig exposing
    ( FeatureConfig
    , decoder
    , default
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias FeatureConfig =
    { pdfOnlyEnabled : Bool
    }


default : FeatureConfig
default =
    { pdfOnlyEnabled = False
    }


decoder : Decoder FeatureConfig
decoder =
    D.succeed FeatureConfig
        |> D.required "pdfOnlyEnabled" D.bool
