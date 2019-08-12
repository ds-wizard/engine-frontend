module KMEditor.Common.KnowledgeModel.AnswerTest exposing (answerDecoderTest)

import KMEditor.Common.KnowledgeModel.Answer as Answer
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
                            "followUpUuids": []
                        }
                        """

                    expected =
                        { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                        , label = "Yes"
                        , advice = Nothing
                        , metricMeasures = []
                        , followUpUuids = []
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
                            "followUpUuids": []
                        }
                        """

                    expected =
                        { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                        , label = "Yes"
                        , advice = Just "Are you sure this is the correct answer?"
                        , metricMeasures = []
                        , followUpUuids = []
                        }
                in
                expectDecoder Answer.decoder raw expected
        , test "should decode answer with metric mesures" <|
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
                            "followUpUuids": []
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
                            "followUpUuids": ["2e4307b9-93b8-4617-b8d1-ba0fa9f15e04"]
                        }
                        """

                    expected =
                        { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                        , label = "Yes"
                        , advice = Nothing
                        , metricMeasures = []
                        , followUpUuids = [ "2e4307b9-93b8-4617-b8d1-ba0fa9f15e04" ]
                        }
                in
                expectDecoder Answer.decoder raw expected
        ]
