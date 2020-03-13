module Wizard.Settings.Common.EditableFeaturesConfig exposing (..)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Wizard.Common.Config.SimpleFeatureConfig as SimpleFeatureConfig exposing (SimpleFeatureConfig)


type alias EditableFeaturesConfig =
    { levels : SimpleFeatureConfig
    , publicQuestionnaire : SimpleFeatureConfig
    , questionnaireAccessibility : SimpleFeatureConfig
    , registration : SimpleFeatureConfig
    }


decoder : Decoder EditableFeaturesConfig
decoder =
    D.succeed EditableFeaturesConfig
        |> D.required "levels" SimpleFeatureConfig.decoder
        |> D.required "publicQuestionnaire" SimpleFeatureConfig.decoder
        |> D.required "questionnaireAccessibility" SimpleFeatureConfig.decoder
        |> D.required "registration" SimpleFeatureConfig.decoder


encode : EditableFeaturesConfig -> E.Value
encode config =
    E.object
        [ ( "levels", SimpleFeatureConfig.encode config.levels )
        , ( "publicQuestionnaire", SimpleFeatureConfig.encode config.publicQuestionnaire )
        , ( "questionnaireAccessibility", SimpleFeatureConfig.encode config.questionnaireAccessibility )
        , ( "registration", SimpleFeatureConfig.encode config.registration )
        ]
