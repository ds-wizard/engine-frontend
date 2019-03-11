module KMEditor.Common.Models.EntitiesTest exposing
    ( answerDecoderTest
    , chapterDecoderTest
    , expertDecoderTest
    , knowledgeModelDecoderTest
    , metricDecoderTest
    ,  metricMeasureDecoderTest
       --    , questionDecoderTest

    , questionDecoderTest
    , referenceDecoderTest
    , tagDecoderTest
    )

import KMEditor.Common.Models.Entities exposing (..)
import Test exposing (..)
import TestUtils exposing (expectDecoder, parametrized)


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
                            "chapters": [],
                            "tags": []
                        }
                        """

                    expected =
                        { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                        , name = "My knowledge model"
                        , chapters = []
                        , tags = []
                        }
                in
                expectDecoder knowledgeModelDecoder raw expected
        , test "should decode knowledge model with chapters" <|
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
                            }],
                            "tags": []
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
                        , tags = []
                        }
                in
                expectDecoder knowledgeModelDecoder raw expected
        , test "should decode knowledge model with tags" <|
            \_ ->
                let
                    raw =
                        """
                        {
                            "uuid": "8a703cfa-450f-421a-8819-875619ccb54d",
                            "name": "My knowledge model",
                            "chapters": [],
                            "tags": [{
                                "uuid": "b5b6ed23-2afa-11e9-b210-d663bd873d93",
                                "name": "Science",
                                "description": null,
                                "color": "#F5A623"
                            }]
                        }
                        """

                    expected =
                        { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                        , name = "My knowledge model"
                        , chapters = []
                        , tags =
                            [ { uuid = "b5b6ed23-2afa-11e9-b210-d663bd873d93"
                              , name = "Science"
                              , description = Nothing
                              , color = "#F5A623"
                              }
                            ]
                        }
                in
                expectDecoder knowledgeModelDecoder raw expected
        ]


tagDecoderTest : Test
tagDecoderTest =
    describe "tagDecoder"
        [ test "should decode tag" <|
            \_ ->
                let
                    raw =
                        """
                        {
                            "uuid": "b5b6ed23-2afa-11e9-b210-d663bd873d93",
                            "name": "Science",
                            "description": null,
                            "color": "#F5A623"
                        }
                        """

                    expected =
                        { uuid = "b5b6ed23-2afa-11e9-b210-d663bd873d93"
                        , name = "Science"
                        , description = Nothing
                        , color = "#F5A623"
                        }
                in
                expectDecoder tagDecoder raw expected
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
                                "questionType": "ValueQuestion",
                                "valueType": "StringValue",
                                "title": "What's your name?",
                                "text": "Fill in your name",
                                "requiredLevel": null,
                                "tagUuids": [],
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
                            [ ValueQuestion
                                { uuid = "2e4307b9-93b8-4617-b8d1-ba0fa9f15e04"
                                , title = "What's your name?"
                                , text = Just "Fill in your name"
                                , requiredLevel = Nothing
                                , tagUuids = []
                                , references = []
                                , experts = []
                                , valueType = StringValueType
                                }
                            ]
                        }
                in
                expectDecoder chapterDecoder raw expected
        ]


questionDecoderTest : Test
questionDecoderTest =
    describe "questionDecoder"
        [ parametrized
            [ ( "StringValue", StringValueType ), ( "NumberValue", NumberValueType ), ( "DateValue", DateValueType ), ( "TextValue", TextValueType ) ]
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
                            "references": [],
                            "experts": []
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
                            , references = []
                            , experts = []
                            , valueType = parsedType
                            }
                in
                expectDecoder questionDecoder raw expected
        , test "should decode question with tag UUIDs" <|
            \_ ->
                let
                    raw =
                        """
                        {
                            "uuid": "8a703cfa-450f-421a-8819-875619ccb54d",
                            "questionType": "ValueQuestion",
                            "valueType": "StringValue",
                            "title": "Can you answer this question?",
                            "text": null,
                            "requiredLevel": 1,
                            "tagUuids": ["563f4528-2ba0-11e9-b210-d663bd873d93", "563f47bc-2ba0-11e9-b210-d663bd873d93"],
                            "answerItemTemplate": null,
                            "answers": null,
                            "references": [],
                            "experts": []
                        }
                        """

                    expected =
                        ValueQuestion
                            { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                            , title = "Can you answer this question?"
                            , text = Nothing
                            , requiredLevel = Just 1
                            , tagUuids = [ "563f4528-2ba0-11e9-b210-d663bd873d93", "563f47bc-2ba0-11e9-b210-d663bd873d93" ]
                            , references = []
                            , experts = []
                            , valueType = StringValueType
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
                            "questionType": "ValueQuestion",
                            "valueType": "StringValue",
                            "title": "Can you answer this question?",
                            "text": "Please answer the question",
                            "requiredLevel": null,
                            "tagUuids": [],
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
                        ValueQuestion
                            { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                            , title = "Can you answer this question?"
                            , text = Just "Please answer the question"
                            , requiredLevel = Nothing
                            , tagUuids = []
                            , references =
                                [ ResourcePageReference
                                    { uuid = "64217c4e-50b3-4230-9224-bf65c4220ab6"
                                    , shortUuid = "atq"
                                    }
                                ]
                            , experts = []
                            , valueType = StringValueType
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
                            "questionType": "ValueQuestion",
                            "valueType": "StringValue",
                            "title": "Can you answer this question?",
                            "text": "Please answer the question",
                            "requiredLevel": 2,
                            "tagUuids": [],
                            "references": [],
                            "experts": [{
                                "uuid": "64217c4e-50b3-4230-9224-bf65c4220ab6",
                                "name": "John Example",
                                "email": "expert@example.com"
                            }]
                        }
                        """

                    expected =
                        ValueQuestion
                            { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                            , title = "Can you answer this question?"
                            , text = Just "Please answer the question"
                            , requiredLevel = Just 2
                            , tagUuids = []
                            , references = []
                            , experts =
                                [ { uuid = "64217c4e-50b3-4230-9224-bf65c4220ab6"
                                  , name = "John Example"
                                  , email = "expert@example.com"
                                  }
                                ]
                            , valueType = StringValueType
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
                            "questionType": "OptionsQuestion",
                            "title": "Can you answer this question?",
                            "text": "Please answer the question",
                            "requiredLevel": null,
                            "tagUuids": [],
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
                        OptionsQuestion
                            { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                            , title = "Can you answer this question?"
                            , text = Just "Please answer the question"
                            , requiredLevel = Nothing
                            , tagUuids = []
                            , references = []
                            , experts = []
                            , answers =
                                [ { uuid = "64217c4e-50b3-4230-9224-bf65c4220ab6"
                                  , label = "Yes"
                                  , advice = Nothing
                                  , metricMeasures = []
                                  , followUps = FollowUps []
                                  }
                                ]
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
                            "questionType": "ListQuestion",
                            "title": "Can you answer this question?",
                            "text": "Please answer the question",
                            "requiredLevel": null,
                            "tagUuids": [],
                            "itemTemplateTitle": "Item",
                            "itemTemplateQuestions": [{
                                "uuid": "2e4307b9-93b8-4617-b8d1-ba0fa9f15e04",
                                "questionType": "ValueQuestion",
                                "valueType": "StringValue",
                                "title": "What's your name?",
                                "text": "Fill in your name",
                                "requiredLevel": null,
                                "tagUuids": [],
                                "references": [],
                                "experts": []
                            }],
                            "references": [],
                            "experts": []
                        }
                        """

                    expected =
                        ListQuestion
                            { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                            , title = "Can you answer this question?"
                            , text = Just "Please answer the question"
                            , requiredLevel = Nothing
                            , tagUuids = []
                            , itemTemplateTitle = "Item"
                            , itemTemplateQuestions =
                                [ ValueQuestion
                                    { uuid = "2e4307b9-93b8-4617-b8d1-ba0fa9f15e04"
                                    , title = "What's your name?"
                                    , text = Just "Fill in your name"
                                    , requiredLevel = Nothing
                                    , tagUuids = []
                                    , references = []
                                    , experts = []
                                    , valueType = StringValueType
                                    }
                                ]
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
                                "questionType": "ValueQuestion",
                                "valueType": "StringValue",
                                "title": "What's your name?",
                                "text": "Fill in your name",
                                "requiredLevel": null,
                                "tagUuids": [],
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
                                [ ValueQuestion
                                    { uuid = "2e4307b9-93b8-4617-b8d1-ba0fa9f15e04"
                                    , title = "What's your name?"
                                    , text = Just "Fill in your name"
                                    , requiredLevel = Nothing
                                    , tagUuids = []
                                    , references = []
                                    , experts = []
                                    , valueType = StringValueType
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
