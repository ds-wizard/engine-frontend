module Wizard.Api.Models.KnowledgeModel.MetricTest exposing (metricDecoderTest)

import Test exposing (Test, describe, test)
import TestUtils exposing (expectDecoder)
import Wizard.Api.Models.KnowledgeModel.Metric as Metric


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
