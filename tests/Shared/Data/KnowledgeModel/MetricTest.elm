module Shared.Data.KnowledgeModel.MetricTest exposing (metricDecoderTest)

import Dict
import Shared.Data.KnowledgeModel.Metric as Metric
import Test exposing (..)
import TestUtils exposing (expectDecoder)


metricDecoderTest : Test
metricDecoderTest =
    describe "metricDecoder"
        [ test "should decode metric" <|
            \_ ->
                let
                    raw =
                        """
                        {
                            "uuid": "8a703cfa-450f-421a-8819-875619ccb54d",
                            "title": "Fairness",
                            "abbreviation": "F",
                            "description": "Fairness describe how fair it is",
                            "annotations": []
                        }
                        """

                    expected =
                        { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                        , title = "Fairness"
                        , abbreviation = Just "F"
                        , description = Just "Fairness describe how fair it is"
                        , annotations = []
                        }
                in
                expectDecoder Metric.decoder raw expected
        ]
