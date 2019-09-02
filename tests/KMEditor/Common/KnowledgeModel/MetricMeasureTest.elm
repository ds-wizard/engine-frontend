module KMEditor.Common.KnowledgeModel.MetricMeasureTest exposing (metricMeasureDecoderTest)

import KMEditor.Common.KnowledgeModel.MetricMeasure as MetricMeasure
import Test exposing (..)
import TestUtils exposing (expectDecoder)


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
