module Wizard.Api.Models.KnowledgeModel.QuestionTest exposing (questionDecoderTest)

import Dict
import Test exposing (Test, describe, test)
import TestUtils exposing (expectDecoder, parametrized)
import Wizard.Api.Models.KnowledgeModel.Question as Question exposing (Question(..))
import Wizard.Api.Models.KnowledgeModel.Question.QuestionValueType exposing (QuestionValueType(..))


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
                            "validations": [],
                            "title": "Can you answer this question?",
                            "text": null,
                            "requiredPhaseUuid": "6832055b-5416-43ae-9896-1d9135ced2c4",
                            "tagUuids": [],
                            "referenceUuids": [],
                            "expertUuids": [],
                            "annotations": []
                        }
                        """
                            |> String.replace "$type" jsonType

                    expected =
                        ValueQuestion
                            { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                            , title = "Can you answer this question?"
                            , text = Nothing
                            , requiredPhaseUuid = Just "6832055b-5416-43ae-9896-1d9135ced2c4"
                            , tagUuids = []
                            , referenceUuids = []
                            , expertUuids = []
                            , annotations = []
                            }
                            { valueType = parsedType
                            , validations = []
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
                            "requiredPhaseUuid": "6832055b-5416-43ae-9896-1d9135ced2c4",
                            "tagUuids": ["563f4528-2ba0-11e9-b210-d663bd873d93", "563f47bc-2ba0-11e9-b210-d663bd873d93"],
                            "referenceUuids": [],
                            "expertUuids": [],
                            "valueType": "StringQuestionValueType",
                            "validations": [],
                            "annotations": []
                        }
                        """

                    expected =
                        ValueQuestion
                            { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                            , title = "Can you answer this question?"
                            , text = Nothing
                            , requiredPhaseUuid = Just "6832055b-5416-43ae-9896-1d9135ced2c4"
                            , tagUuids = [ "563f4528-2ba0-11e9-b210-d663bd873d93", "563f47bc-2ba0-11e9-b210-d663bd873d93" ]
                            , referenceUuids = []
                            , expertUuids = []
                            , annotations = []
                            }
                            { valueType = StringQuestionValueType
                            , validations = []
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
                            "validations": [],
                            "title": "Can you answer this question?",
                            "text": "Please answer the question",
                            "requiredPhaseUuid": null,
                            "tagUuids": [],
                            "referenceUuids": ["64217c4e-50b3-4230-9224-bf65c4220ab6"],
                            "expertUuids": [],
                            "annotations": []
                        }
                        """

                    expected =
                        ValueQuestion
                            { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                            , title = "Can you answer this question?"
                            , text = Just "Please answer the question"
                            , requiredPhaseUuid = Nothing
                            , tagUuids = []
                            , referenceUuids = [ "64217c4e-50b3-4230-9224-bf65c4220ab6" ]
                            , expertUuids = []
                            , annotations = []
                            }
                            { valueType = StringQuestionValueType
                            , validations = []
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
                            "validations": [],
                            "title": "Can you answer this question?",
                            "text": "Please answer the question",
                            "requiredPhaseUuid": "0948bd26-d985-4549-b7c8-95e9061d6413",
                            "tagUuids": [],
                            "referenceUuids": [],
                            "expertUuids": ["64217c4e-50b3-4230-9224-bf65c4220ab6"],
                            "annotations": []
                        }
                        """

                    expected =
                        ValueQuestion
                            { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                            , title = "Can you answer this question?"
                            , text = Just "Please answer the question"
                            , requiredPhaseUuid = Just "0948bd26-d985-4549-b7c8-95e9061d6413"
                            , tagUuids = []
                            , referenceUuids = []
                            , expertUuids = [ "64217c4e-50b3-4230-9224-bf65c4220ab6" ]
                            , annotations = []
                            }
                            { valueType = StringQuestionValueType
                            , validations = []
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
                            "requiredPhaseUuid": null,
                            "tagUuids": [],
                            "answerUuids": ["64217c4e-50b3-4230-9224-bf65c4220ab6"],
                            "referenceUuids": [],
                            "expertUuids": [],
                            "annotations": []
                        }
                        """

                    expected =
                        OptionsQuestion
                            { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                            , title = "Can you answer this question?"
                            , text = Just "Please answer the question"
                            , requiredPhaseUuid = Nothing
                            , tagUuids = []
                            , referenceUuids = []
                            , expertUuids = []
                            , annotations = []
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
                            "requiredPhaseUuid": null,
                            "tagUuids": [],
                            "itemTemplateQuestionUuids": ["2e4307b9-93b8-4617-b8d1-ba0fa9f15e04"],
                            "referenceUuids": [],
                            "expertUuids": [],
                            "annotations": []
                        }
                        """

                    expected =
                        ListQuestion
                            { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                            , title = "Can you answer this question?"
                            , text = Just "Please answer the question"
                            , requiredPhaseUuid = Nothing
                            , tagUuids = []
                            , referenceUuids = []
                            , expertUuids = []
                            , annotations = []
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
                            "requiredPhaseUuid": null,
                            "tagUuids": [],
                            "referenceUuids": [],
                            "expertUuids": [],
                            "integrationUuid": "b50bf5ce-2fc3-4779-9756-5f176c233374",
                            "props": {
                                "prop": "value"
                            },
                            "annotations": []
                        }
                        """

                    expected =
                        IntegrationQuestion
                            { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                            , title = "Can you answer this question?"
                            , text = Just "Please answer the question"
                            , requiredPhaseUuid = Nothing
                            , tagUuids = []
                            , referenceUuids = []
                            , expertUuids = []
                            , annotations = []
                            }
                            { integrationUuid = "b50bf5ce-2fc3-4779-9756-5f176c233374"
                            , props = Dict.fromList [ ( "prop", "value" ) ]
                            }
                in
                expectDecoder Question.decoder raw expected
        , test "should decode item select question type" <|
            \_ ->
                let
                    raw =
                        """
                        {
                            "uuid": "8a703cfa-450f-421a-8819-875619ccb54d",
                            "questionType": "ItemSelectQuestion",
                            "title": "Can you answer this question?",
                            "text": "Please answer the question",
                            "requiredPhaseUuid": null,
                            "tagUuids": [],
                            "referenceUuids": [],
                            "expertUuids": [],
                            "listQuestionUuid": "b50bf5ce-2fc3-4779-9756-5f176c233374",
                            "annotations": []
                        }
                        """

                    expected =
                        ItemSelectQuestion
                            { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                            , title = "Can you answer this question?"
                            , text = Just "Please answer the question"
                            , requiredPhaseUuid = Nothing
                            , tagUuids = []
                            , referenceUuids = []
                            , expertUuids = []
                            , annotations = []
                            }
                            { listQuestionUuid = Just "b50bf5ce-2fc3-4779-9756-5f176c233374"
                            }
                in
                expectDecoder Question.decoder raw expected
        ]
