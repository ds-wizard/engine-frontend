module Shared.Data.KnowledgeModel.AnswerTest exposing (answerDecoderTest)

import Dict
import Shared.Data.KnowledgeModel.Answer as Answer
import Test exposing (..)
import TestUtils exposing (expectDecoder)


answerDecoderTest : Test
answerDecoderTest =
    describe "answerDecoder"
        [ test "should decode simple answer" <|
            \_ ->
                let
                    raw =
                        """
                        {
                            "uuid": "8a703cfa-450f-421a-8819-875619ccb54d",
                            "label": "Yes",
                            "advice": null,
                            "metricMeasures": [],
                            "followUpUuids": [],
                            "annotations": []
                        }
                        """

                    expected =
                        { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                        , label = "Yes"
                        , advice = Nothing
                        , metricMeasures = []
                        , followUpUuids = []
                        , annotations = []
                        }
                in
                expectDecoder Answer.decoder raw expected
        , test "should decode answer with advice" <|
            \_ ->
                let
                    raw =
                        """
                        {
                            "uuid": "8a703cfa-450f-421a-8819-875619ccb54d",
                            "label": "Yes",
                            "advice": "Are you sure this is the correct answer?",
                            "metricMeasures": [],
                            "followUpUuids": [],
                            "annotations": []
                        }
                        """

                    expected =
                        { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                        , label = "Yes"
                        , advice = Just "Are you sure this is the correct answer?"
                        , metricMeasures = []
                        , followUpUuids = []
                        , annotations = []
                        }
                in
                expectDecoder Answer.decoder raw expected
        , test "should decode answer with metric measures" <|
            \_ ->
                let
                    raw =
                        """
                        {
                            "uuid": "8a703cfa-450f-421a-8819-875619ccb54d",
                            "label": "Yes",
                            "advice": null,
                            "metricMeasures": [{
                                "metricUuid": "2e4307b9-93b8-4617-b8d1-ba0fa9f15e04",
                                "measure": 0.3,
                                "weight": 0.8
                            }],
                            "followUpUuids": [],
                            "annotations": []
                        }
                        """

                    expected =
                        { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                        , label = "Yes"
                        , advice = Nothing
                        , metricMeasures =
                            [ { metricUuid = "2e4307b9-93b8-4617-b8d1-ba0fa9f15e04"
                              , measure = 0.3
                              , weight = 0.8
                              }
                            ]
                        , followUpUuids = []
                        , annotations = []
                        }
                in
                expectDecoder Answer.decoder raw expected
        , test "should decode answer with follow up questions" <|
            \_ ->
                let
                    raw =
                        """
                        {
                            "uuid": "8a703cfa-450f-421a-8819-875619ccb54d",
                            "label": "Yes",
                            "advice": null,
                            "metricMeasures": [],
                            "followUpUuids": ["2e4307b9-93b8-4617-b8d1-ba0fa9f15e04"],
                            "annotations": []
                        }
                        """

                    expected =
                        { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                        , label = "Yes"
                        , advice = Nothing
                        , metricMeasures = []
                        , followUpUuids = [ "2e4307b9-93b8-4617-b8d1-ba0fa9f15e04" ]
                        , annotations = []
                        }
                in
                expectDecoder Answer.decoder raw expected
        ]
