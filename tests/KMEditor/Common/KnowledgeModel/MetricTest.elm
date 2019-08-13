module KMEditor.Common.KnowledgeModel.MetricTest exposing (metricDecoderTest)

import KMEditor.Common.KnowledgeModel.Metric as Metric
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
                            "description": "Fairness describe how fair it is"
                        }
                        """

                    expected =
                        { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                        , title = "Fairness"
                        , abbreviation = "F"
                        , description = "Fairness describe how fair it is"
                        }
                in
                expectDecoder Metric.decoder raw expected
        ]
