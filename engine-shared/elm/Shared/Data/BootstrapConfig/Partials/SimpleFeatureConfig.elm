module Shared.Data.BootstrapConfig.Partials.SimpleFeatureConfig exposing
    ( SimpleFeatureConfig
    , decoder
    , init
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias SimpleFeatureConfig =
    { enabled : Bool }


init : Bool -> SimpleFeatureConfig
init value =
    { enabled = value }


decoder : Decoder SimpleFeatureConfig
decoder =
    D.succeed SimpleFeatureConfig
        |> D.required "enabled" D.bool
