module KMEditor.Common.Models.EntitiesTest exposing (..)

import Expect
import Json.Decode as Decode exposing (..)
import KMEditor.Common.Models.Entities exposing (..)
import Test exposing (..)
import TestUtils exposing (expectDecoder, parametrized)
import Utils exposing (replace)


knowledgeModelDecoderTest : Test
knowledgeModelDecoderTest =
    describe "knowledgeModelDecoder"
        [ test "should decode simple knowledge model" <|
            \_ ->
                let
                    raw =
                        """
                        {
                            "uuid": "8a703cfa-450f-421a-8819-875619ccb54d",
                            "name": "My knowledge model",
                            "chapters": []
                        }
                        """

                    expected =
                        { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                        , name = "My knowledge model"
                        , chapters = []
                        }
                in
                expectDecoder knowledgeModelDecoder raw expected
        , test "should decode knowledge model with questions" <|
            \_ ->
                let
                    raw =
                        """
                        {
                            "uuid": "8a703cfa-450f-421a-8819-875619ccb54d",
                            "name": "My knowledge model",
                            "chapters": [{
                                "uuid": "2e4307b9-93b8-4617-b8d1-ba0fa9f15e04",
                                "title": "Chapter 1",
                                "text": "This chapter is empty",
                                "questions": []
                            }]
                        }
                        """

                    expected =
                        { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                        , name = "My knowledge model"
                        , chapters =
                            [ { uuid = "2e4307b9-93b8-4617-b8d1-ba0fa9f15e04"
                              , title = "Chapter 1"
                              , text = "This chapter is empty"
                              , questions = []
                              }
                            ]
                        }
                in
                expectDecoder knowledgeModelDecoder raw expected
        ]


chapterDecoderTest : Test
chapterDecoderTest =
    describe "chapterDecoder"
        [ test "should decode simple chapter" <|
            \_ ->
                let
                    raw =
                        """
                        {
                            "uuid": "8a703cfa-450f-421a-8819-875619ccb54d",
                            "title": "Chapter 1",
                            "text": "This chapter is empty",
                            "questions": []
                        }
                        """

                    expected =
                        { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                        , title = "Chapter 1"
                        , text = "This chapter is empty"
                        , questions = []
                        }
                in
                expectDecoder chapterDecoder raw expected
        , test "should decode chapter with questions" <|
            \_ ->
                let
                    raw =
                        """
                        {
                            "uuid": "8a703cfa-450f-421a-8819-875619ccb54d",
                            "title": "Chapter 1",
                            "text": "This chapter is empty",
                            "questions": [{
                                "uuid": "2e4307b9-93b8-4617-b8d1-ba0fa9f15e04",
                                "type": "string",
                                "title": "What's your name?",
                                "text": "Fill in your name",
                                "requiredLevel": null,
                                "answerItemTemplate": null,
                                "answers": null,
                                "references": [],
                                "experts": []
                            }]
                        }
                        """

                    expected =
                        { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                        , title = "Chapter 1"
                        , text = "This chapter is empty"
                        , questions =
                            [ { uuid = "2e4307b9-93b8-4617-b8d1-ba0fa9f15e04"
                              , type_ = "string"
                              , title = "What's your name?"
                              , text = Just "Fill in your name"
                              , requiredLevel = Nothing
                              , answerItemTemplate = Nothing
                              , answers = Nothing
                              , references = []
                              , experts = []
                              }
                            ]
                        }
                in
                expectDecoder chapterDecoder raw expected
        ]


questionDecoderTest : Test
questionDecoderTest =
    describe "questionDecoder"
        [ parametrized [ "string", "number", "date", "text" ] "should decode simple types question" <|
            \type_ ->
                let
                    raw =
                        """
                        {
                            "uuid": "8a703cfa-450f-421a-8819-875619ccb54d",
                            "type": "$type",
                            "title": "Can you answer this question?",
                            "text": null,
                            "requiredLevel": 1,
                            "answerItemTemplate": null,
                            "answers": null,
                            "references": [],
                            "experts": []
                        }
                        """
                            |> replace "$type" type_

                    expected =
                        { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                        , type_ = type_
                        , title = "Can you answer this question?"
                        , text = Nothing
                        , requiredLevel = Just 1
                        , answerItemTemplate = Nothing
                        , answers = Nothing
                        , references = []
                        , experts = []
                        }
                in
                expectDecoder questionDecoder raw expected
        , test "should decode question with references" <|
            \_ ->
                let
                    raw =
                        """
                        {
                            "uuid": "8a703cfa-450f-421a-8819-875619ccb54d",
                            "type": "string",
                            "title": "Can you answer this question?",
                            "text": "Please answer the question",
                            "requiredLevel": null,
                            "answerItemTemplate": null,
                            "answers": null,
                            "references": [{
                                "referenceType": "ResourcePageReference",
                                "uuid": "64217c4e-50b3-4230-9224-bf65c4220ab6",
                                "shortUuid": "atq"
                            }],
                            "experts": []
                        }
                        """

                    expected =
                        { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                        , type_ = "string"
                        , title = "Can you answer this question?"
                        , text = Just "Please answer the question"
                        , requiredLevel = Nothing
                        , answerItemTemplate = Nothing
                        , answers = Nothing
                        , references =
                            [ ResourcePageReference
                                { uuid = "64217c4e-50b3-4230-9224-bf65c4220ab6"
                                , shortUuid = "atq"
                                }
                            ]
                        , experts = []
                        }
                in
                expectDecoder questionDecoder raw expected
        , test "should decode question with experts" <|
            \_ ->
                let
                    raw =
                        """
                        {
                            "uuid": "8a703cfa-450f-421a-8819-875619ccb54d",
                            "type": "string",
                            "title": "Can you answer this question?",
                            "text": "Please answer the question",
                            "requiredLevel": 2,
                            "answerItemTemplate": null,
                            "answers": null,
                            "references": [],
                            "experts": [{
                                "uuid": "64217c4e-50b3-4230-9224-bf65c4220ab6",
                                "name": "John Example",
                                "email": "expert@example.com"
                            }]
                        }
                        """

                    expected =
                        { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                        , type_ = "string"
                        , title = "Can you answer this question?"
                        , text = Just "Please answer the question"
                        , requiredLevel = Just 2
                        , answerItemTemplate = Nothing
                        , answers = Nothing
                        , references = []
                        , experts =
                            [ { uuid = "64217c4e-50b3-4230-9224-bf65c4220ab6"
                              , name = "John Example"
                              , email = "expert@example.com"
                              }
                            ]
                        }
                in
                expectDecoder questionDecoder raw expected
        , test "should decode options question type" <|
            \_ ->
                let
                    raw =
                        """
                        {
                            "uuid": "8a703cfa-450f-421a-8819-875619ccb54d",
                            "type": "options",
                            "title": "Can you answer this question?",
                            "text": "Please answer the question",
                            "requiredLevel": null,
                            "answerItemTemplate": null,
                            "answers": [{
                                "uuid": "64217c4e-50b3-4230-9224-bf65c4220ab6",
                                "label": "Yes",
                                "advice": null,
                                "metricMeasures": [],
                                "followUps": []
                            }],
                            "references": [],
                            "experts": []
                        }
                        """

                    expected =
                        { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                        , type_ = "options"
                        , title = "Can you answer this question?"
                        , text = Just "Please answer the question"
                        , requiredLevel = Nothing
                        , answerItemTemplate = Nothing
                        , answers =
                            Just
                                [ { uuid = "64217c4e-50b3-4230-9224-bf65c4220ab6"
                                  , label = "Yes"
                                  , advice = Nothing
                                  , metricMeasures = []
                                  , followUps = FollowUps []
                                  }
                                ]
                        , references = []
                        , experts = []
                        }
                in
                expectDecoder questionDecoder raw expected
        , test "should decode list question type" <|
            \_ ->
                let
                    raw =
                        """
                        {
                            "uuid": "8a703cfa-450f-421a-8819-875619ccb54d",
                            "type": "list",
                            "title": "Can you answer this question?",
                            "text": "Please answer the question",
                            "requiredLevel": null,
                            "answerItemTemplate": {
                                "title": "Item",
                                "questions": [{
                                    "uuid": "2e4307b9-93b8-4617-b8d1-ba0fa9f15e04",
                                    "type": "string",
                                    "title": "What's your name?",
                                    "text": "Fill in your name",
                                    "requiredLevel": null,
                                    "answerItemTemplate": null,
                                    "answers": null,
                                    "references": [],
                                    "experts": []
                                }]
                            },
                            "answers": null,
                            "references": [],
                            "experts": []
                        }
                        """

                    expected =
                        { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                        , type_ = "list"
                        , title = "Can you answer this question?"
                        , text = Just "Please answer the question"
                        , requiredLevel = Nothing
                        , answerItemTemplate =
                            Just
                                { title = "Item"
                                , questions =
                                    AnswerItemTemplateQuestions
                                        [ { uuid = "2e4307b9-93b8-4617-b8d1-ba0fa9f15e04"
                                          , type_ = "string"
                                          , title = "What's your name?"
                                          , text = Just "Fill in your name"
                                          , requiredLevel = Nothing
                                          , answerItemTemplate = Nothing
                                          , answers = Nothing
                                          , references = []
                                          , experts = []
                                          }
                                        ]
                                }
                        , answers = Nothing
                        , references = []
                        , experts = []
                        }
                in
                expectDecoder questionDecoder raw expected
        ]


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
                            "followUps": []
                        }
                        """

                    expected =
                        { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                        , label = "Yes"
                        , advice = Nothing
                        , metricMeasures = []
                        , followUps = FollowUps []
                        }
                in
                expectDecoder answerDecoder raw expected
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
                            "followUps": []
                        }
                        """

                    expected =
                        { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                        , label = "Yes"
                        , advice = Just "Are you sure this is the correct answer?"
                        , metricMeasures = []
                        , followUps = FollowUps []
                        }
                in
                expectDecoder answerDecoder raw expected
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
                            "followUps": []
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
                        , followUps = FollowUps []
                        }
                in
                expectDecoder answerDecoder raw expected
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
                            "followUps": [{
                                "uuid": "2e4307b9-93b8-4617-b8d1-ba0fa9f15e04",
                                "type": "string",
                                "title": "What's your name?",
                                "text": "Fill in your name",
                                "requiredLevel": null,
                                "answerItemTemplate": null,
                                "answers": null,
                                "references": [],
                                "experts": []
                            }]
                        }
                        """

                    expected =
                        { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                        , label = "Yes"
                        , advice = Nothing
                        , metricMeasures = []
                        , followUps =
                            FollowUps
                                [ { uuid = "2e4307b9-93b8-4617-b8d1-ba0fa9f15e04"
                                  , type_ = "string"
                                  , title = "What's your name?"
                                  , text = Just "Fill in your name"
                                  , requiredLevel = Nothing
                                  , answerItemTemplate = Nothing
                                  , answers = Nothing
                                  , references = []
                                  , experts = []
                                  }
                                ]
                        }
                in
                expectDecoder answerDecoder raw expected
        ]


referenceDecoderTest : Test
referenceDecoderTest =
    describe "referenceDecoder"
        [ test "should decode ResourcePageReference" <|
            \_ ->
                let
                    raw =
                        """
                        {
                            "referenceType": "ResourcePageReference",
                            "uuid": "8a703cfa-450f-421a-8819-875619ccb54d",
                            "shortUuid": "atq"
                        }
                        """

                    expected =
                        ResourcePageReference
                            { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                            , shortUuid = "atq"
                            }
                in
                expectDecoder referenceDecoder raw expected
        , test "should decode URLReference" <|
            \_ ->
                let
                    raw =
                        """
                        {
                            "referenceType": "URLReference",
                            "uuid": "8a703cfa-450f-421a-8819-875619ccb54d",
                            "url": "http://example.com",
                            "label": "See also"
                        }
                        """

                    expected =
                        URLReference
                            { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                            , url = "http://example.com"
                            , label = "See also"
                            }
                in
                expectDecoder referenceDecoder raw expected
        , test "should decode CrossReference" <|
            \_ ->
                let
                    raw =
                        """
                        {
                            "referenceType": "CrossReference",
                            "uuid": "8a703cfa-450f-421a-8819-875619ccb54d",
                            "targetUuid": "64217c4e-50b3-4230-9224-bf65c4220ab6",
                            "description": "See also"
                        }
                        """

                    expected =
                        CrossReference
                            { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                            , targetUuid = "64217c4e-50b3-4230-9224-bf65c4220ab6"
                            , description = "See also"
                            }
                in
                expectDecoder referenceDecoder raw expected
        ]


expertDecoderTest : Test
expertDecoderTest =
    describe "expertDecoder"
        [ test "should decode expert" <|
            \_ ->
                let
                    raw =
                        """
                        {
                            "uuid": "8a703cfa-450f-421a-8819-875619ccb54d",
                            "name": "John Example",
                            "email": "expert@example.com"
                        }
                        """

                    expected =
                        { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                        , name = "John Example"
                        , email = "expert@example.com"
                        }
                in
                expectDecoder expertDecoder raw expected
        ]


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
                expectDecoder metricDecoder raw expected
        ]


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
                expectDecoder metricMeasureDecoder raw expected
        ]
