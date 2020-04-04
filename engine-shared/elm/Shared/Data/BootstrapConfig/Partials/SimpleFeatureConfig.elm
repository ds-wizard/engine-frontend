module Shared.Data.BootstrapConfig.Partials.SimpleFeatureConfig exposing
    ( SimpleFeatureConfig
    , decoder
    , init
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type SimpleFeatureConfig
    = SimpleFeatureConfig Internals


type alias Internals =
    { enabled : Bool }


init : Bool -> SimpleFeatureConfig
init value =
    SimpleFeatureConfig { enabled = value }


decoder : Decoder SimpleFeatureConfig
decoder =
    D.succeed Internals
        |> D.required "enabled" D.bool
        |> D.map SimpleFeatureConfig
