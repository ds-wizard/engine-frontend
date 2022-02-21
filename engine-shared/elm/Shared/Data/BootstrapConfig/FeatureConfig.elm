module Shared.Data.BootstrapConfig.FeatureConfig exposing
    ( FeatureConfig
    , decoder
    , default
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias FeatureConfig =
    { clientCustomizationEnabled : Bool
    , pdfOnlyEnabled : Bool
    }


default : FeatureConfig
default =
    { clientCustomizationEnabled = False
    , pdfOnlyEnabled = False
    }


decoder : Decoder FeatureConfig
decoder =
    D.succeed FeatureConfig
        |> D.required "clientCustomizationEnabled" D.bool
        |> D.required "pdfOnlyEnabled" D.bool
