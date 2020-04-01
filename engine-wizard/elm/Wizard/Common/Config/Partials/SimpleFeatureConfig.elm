module Wizard.Common.Config.Partials.SimpleFeatureConfig exposing
    ( SimpleFeatureConfig
    , decoder
    , enabled
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias SimpleFeatureConfig =
    { enabled : Bool }


decoder : Decoder SimpleFeatureConfig
decoder =
    D.succeed SimpleFeatureConfig
        |> D.required "enabled" D.bool


encode : SimpleFeatureConfig -> E.Value
encode config =
    E.object
        [ ( "enabled", E.bool config.enabled ) ]


enabled : SimpleFeatureConfig
enabled =
    { enabled = True }
