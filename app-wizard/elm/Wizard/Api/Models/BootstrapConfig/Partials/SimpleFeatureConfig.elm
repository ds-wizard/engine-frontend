module Wizard.Api.Models.BootstrapConfig.Partials.SimpleFeatureConfig exposing
    ( SimpleFeatureConfig
    , decoder
    , encode
    , field
    , init
    , validation
    )

import Form.Field as Field exposing (Field)
import Form.Validate as V exposing (Validation)
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


init : Bool -> SimpleFeatureConfig
init =
    SimpleFeatureConfig


field : SimpleFeatureConfig -> Field
field =
    .enabled >> Field.bool


validation : Validation e SimpleFeatureConfig
validation =
    V.bool |> V.map (\value -> { enabled = value })
