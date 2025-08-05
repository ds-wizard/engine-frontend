module Wizard.Api.Models.KnowledgeModel.MetricMeasureTest exposing (metricMeasureDecoderTest)

import Test exposing (Test, describe, test)
import TestUtils exposing (expectDecoder)
import Wizard.Api.Models.KnowledgeModel.MetricMeasure as MetricMeasure


metricMeasureDecoderTest : Test
metricMeasureDecoderTest =
    describe "metricMeasureDecoder"
        [ test "should decode metric measure" <|
            \_ ->
                let
                    raw =
                        """
                        {
                            "metricUuid": "8a703cfa-450f-421a-8819-875619ccb54d",
                            "measure": 0.7,
                            "weight": 0.5
                        }
                        """

                    expected =
                        { metricUuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                        , measure = 0.7
                        , weight = 0.5
                        }
                in
                expectDecoder MetricMeasure.decoder raw expected
        ]
