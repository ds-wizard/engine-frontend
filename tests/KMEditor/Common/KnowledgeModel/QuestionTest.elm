module KMEditor.Common.KnowledgeModel.QuestionTest exposing (questionDecoderTest)

import Dict
import KMEditor.Common.KnowledgeModel.Question as Question exposing (Question(..))
import KMEditor.Common.KnowledgeModel.Question.QuestionValueType exposing (QuestionValueType(..))
import Test exposing (..)
import TestUtils exposing (expectDecoder, parametrized)


questionDecoderTest : Test
questionDecoderTest =
    describe "questionDecoder"
        [ parametrized
            [ ( "StringQuestionValueType", StringQuestionValueType )
            , ( "NumberQuestionValueType", NumberQuestionValueType )
            , ( "DateQuestionValueType", DateQuestionValueType )
            , ( "TextQuestionValueType", TextQuestionValueType )
            ]
            "should decode value types question"
          <|
            \( jsonType, parsedType ) ->
                let
                    raw =
                        """
                        {
                            "uuid": "8a703cfa-450f-421a-8819-875619ccb54d",
                            "questionType": "ValueQuestion",
                            "valueType": "$type",
                            "title": "Can you answer this question?",
                            "text": null,
                            "requiredLevel": 1,
                            "tagUuids": [],
                            "referenceUuids": [],
                            "expertUuids": []
                        }
                        """
                            |> String.replace "$type" jsonType

                    expected =
                        ValueQuestion
                            { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                            , title = "Can you answer this question?"
                            , text = Nothing
                            , requiredLevel = Just 1
                            , tagUuids = []
                            , referenceUuids = []
                            , expertUuids = []
                            }
                            { valueType = parsedType
                            }
                in
                expectDecoder Question.decoder raw expected
        , test "should decode question with tag UUIDs" <|
            \_ ->
                let
                    raw =
                        """
                        {
                            "uuid": "8a703cfa-450f-421a-8819-875619ccb54d",
                            "questionType": "ValueQuestion",
                            "title": "Can you answer this question?",
                            "text": null,
                            "requiredLevel": 1,
                            "tagUuids": ["563f4528-2ba0-11e9-b210-d663bd873d93", "563f47bc-2ba0-11e9-b210-d663bd873d93"],
                            "referenceUuids": [],
                            "expertUuids": [],
                            "valueType": "StringQuestionValueType"
                        }
                        """

                    expected =
                        ValueQuestion
                            { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                            , title = "Can you answer this question?"
                            , text = Nothing
                            , requiredLevel = Just 1
                            , tagUuids = [ "563f4528-2ba0-11e9-b210-d663bd873d93", "563f47bc-2ba0-11e9-b210-d663bd873d93" ]
                            , referenceUuids = []
                            , expertUuids = []
                            }
                            { valueType = StringQuestionValueType
                            }
                in
                expectDecoder Question.decoder raw expected
        , test "should decode question with references" <|
            \_ ->
                let
                    raw =
                        """
                        {
                            "uuid": "8a703cfa-450f-421a-8819-875619ccb54d",
                            "questionType": "ValueQuestion",
                            "valueType": "StringQuestionValueType",
                            "title": "Can you answer this question?",
                            "text": "Please answer the question",
                            "requiredLevel": null,
                            "tagUuids": [],
                            "referenceUuids": ["64217c4e-50b3-4230-9224-bf65c4220ab6"],
                            "expertUuids": []
                        }
                        """

                    expected =
                        ValueQuestion
                            { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                            , title = "Can you answer this question?"
                            , text = Just "Please answer the question"
                            , requiredLevel = Nothing
                            , tagUuids = []
                            , referenceUuids = [ "64217c4e-50b3-4230-9224-bf65c4220ab6" ]
                            , expertUuids = []
                            }
                            { valueType = StringQuestionValueType
                            }
                in
                expectDecoder Question.decoder raw expected
        , test "should decode question with experts" <|
            \_ ->
                let
                    raw =
                        """
                        {
                            "uuid": "8a703cfa-450f-421a-8819-875619ccb54d",
                            "questionType": "ValueQuestion",
                            "valueType": "StringQuestionValueType",
                            "title": "Can you answer this question?",
                            "text": "Please answer the question",
                            "requiredLevel": 2,
                            "tagUuids": [],
                            "referenceUuids": [],
                            "expertUuids": ["64217c4e-50b3-4230-9224-bf65c4220ab6"]
                        }
                        """

                    expected =
                        ValueQuestion
                            { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                            , title = "Can you answer this question?"
                            , text = Just "Please answer the question"
                            , requiredLevel = Just 2
                            , tagUuids = []
                            , referenceUuids = []
                            , expertUuids = [ "64217c4e-50b3-4230-9224-bf65c4220ab6" ]
                            }
                            { valueType = StringQuestionValueType
                            }
                in
                expectDecoder Question.decoder raw expected
        , test "should decode options question type" <|
            \_ ->
                let
                    raw =
                        """
                        {
                            "uuid": "8a703cfa-450f-421a-8819-875619ccb54d",
                            "questionType": "OptionsQuestion",
                            "title": "Can you answer this question?",
                            "text": "Please answer the question",
                            "requiredLevel": null,
                            "tagUuids": [],
                            "answerUuids": ["64217c4e-50b3-4230-9224-bf65c4220ab6"],
                            "referenceUuids": [],
                            "expertUuids": []
                        }
                        """

                    expected =
                        OptionsQuestion
                            { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                            , title = "Can you answer this question?"
                            , text = Just "Please answer the question"
                            , requiredLevel = Nothing
                            , tagUuids = []
                            , referenceUuids = []
                            , expertUuids = []
                            }
                            { answerUuids = [ "64217c4e-50b3-4230-9224-bf65c4220ab6" ]
                            }
                in
                expectDecoder Question.decoder raw expected
        , test "should decode list question type" <|
            \_ ->
                let
                    raw =
                        """
                        {
                            "uuid": "8a703cfa-450f-421a-8819-875619ccb54d",
                            "questionType": "ListQuestion",
                            "title": "Can you answer this question?",
                            "text": "Please answer the question",
                            "requiredLevel": null,
                            "tagUuids": [],
                            "itemTemplateQuestionUuids": ["2e4307b9-93b8-4617-b8d1-ba0fa9f15e04"],
                            "referenceUuids": [],
                            "expertUuids": []
                        }
                        """

                    expected =
                        ListQuestion
                            { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                            , title = "Can you answer this question?"
                            , text = Just "Please answer the question"
                            , requiredLevel = Nothing
                            , tagUuids = []
                            , referenceUuids = []
                            , expertUuids = []
                            }
                            { itemTemplateQuestionUuids = [ "2e4307b9-93b8-4617-b8d1-ba0fa9f15e04" ]
                            }
                in
                expectDecoder Question.decoder raw expected
        , test "should decode integration question type" <|
            \_ ->
                let
                    raw =
                        """
                        {
                            "uuid": "8a703cfa-450f-421a-8819-875619ccb54d",
                            "questionType": "IntegrationQuestion",
                            "title": "Can you answer this question?",
                            "text": "Please answer the question",
                            "requiredLevel": null,
                            "tagUuids": [],
                            "referenceUuids": [],
                            "expertUuids": [],
                            "integrationUuid": "b50bf5ce-2fc3-4779-9756-5f176c233374",
                            "props": {
                                "prop": "value"
                            }
                        }
                        """

                    expected =
                        IntegrationQuestion
                            { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                            , title = "Can you answer this question?"
                            , text = Just "Please answer the question"
                            , requiredLevel = Nothing
                            , tagUuids = []
                            , referenceUuids = []
                            , expertUuids = []
                            }
                            { integrationUuid = "b50bf5ce-2fc3-4779-9756-5f176c233374"
                            , props = Dict.fromList [ ( "prop", "value" ) ]
                            }
                in
                expectDecoder Question.decoder raw expected
        ]
