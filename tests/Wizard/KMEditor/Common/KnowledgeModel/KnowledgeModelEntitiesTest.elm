module Wizard.KMEditor.Common.KnowledgeModel.KnowledgeModelEntitiesTest exposing (knowledgeModelEntitiesDecoderTest)

import Dict
import Test exposing (..)
import TestUtils exposing (expectDecoder)
import Wizard.KMEditor.Common.KnowledgeModel.KnowledgeModelEntities as KnowledgeModelEntities
import Wizard.KMEditor.Common.KnowledgeModel.Question exposing (Question(..))
import Wizard.KMEditor.Common.KnowledgeModel.Reference exposing (Reference(..))


knowledgeModelEntitiesDecoderTest : Test
knowledgeModelEntitiesDecoderTest =
    describe "KnowledgeModelEntities.decoder"
        [ test "should decode empty" <|
            \_ ->
                let
                    raw =
                        """
                        {
                            "chapters": {},
                            "questions": {},
                            "answers": {},
                            "experts": {},
                            "references": {},
                            "integrations": {},
                            "tags": {}
                        }
                        """

                    expected =
                        { chapters = Dict.empty
                        , questions = Dict.empty
                        , answers = Dict.empty
                        , experts = Dict.empty
                        , references = Dict.empty
                        , integrations = Dict.empty
                        , tags = Dict.empty
                        }
                in
                expectDecoder KnowledgeModelEntities.decoder raw expected
        , test "should decode chapters" <|
            \_ ->
                let
                    raw =
                        """
                        {
                            "chapters": {
                                "2e4307b9-93b8-4617-b8d1-ba0fa9f15e04": {
                                    "uuid": "2e4307b9-93b8-4617-b8d1-ba0fa9f15e04",
                                    "title": "Chapter 1",
                                    "text": "This chapter is empty",
                                    "questionUuids": []
                                }
                            },
                            "questions": {},
                            "answers": {},
                            "experts": {},
                            "references": {},
                            "integrations": {},
                            "tags": {}
                        }
                        """

                    expected =
                        { chapters =
                            Dict.fromList
                                [ ( "2e4307b9-93b8-4617-b8d1-ba0fa9f15e04"
                                  , { uuid = "2e4307b9-93b8-4617-b8d1-ba0fa9f15e04"
                                    , title = "Chapter 1"
                                    , text = Just "This chapter is empty"
                                    , questionUuids = []
                                    }
                                  )
                                ]
                        , questions = Dict.empty
                        , answers = Dict.empty
                        , experts = Dict.empty
                        , references = Dict.empty
                        , integrations = Dict.empty
                        , tags = Dict.empty
                        }
                in
                expectDecoder KnowledgeModelEntities.decoder raw expected
        , test "should decode questions" <|
            \_ ->
                let
                    raw =
                        """
                        {
                            "chapters": {},
                            "questions": {
                                "8a703cfa-450f-421a-8819-875619ccb54d": {
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
                            },
                            "answers": {},
                            "experts": {},
                            "references": {},
                            "integrations": {},
                            "tags": {}
                        }
                        """

                    expected =
                        { chapters = Dict.empty
                        , questions =
                            Dict.fromList
                                [ ( "8a703cfa-450f-421a-8819-875619ccb54d"
                                  , IntegrationQuestion
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
                                  )
                                ]
                        , answers = Dict.empty
                        , experts = Dict.empty
                        , references = Dict.empty
                        , integrations = Dict.empty
                        , tags = Dict.empty
                        }
                in
                expectDecoder KnowledgeModelEntities.decoder raw expected
        , test "should decode answers" <|
            \_ ->
                let
                    raw =
                        """
                        {
                            "chapters": {},
                            "questions": {},
                            "answers": {
                                "8a703cfa-450f-421a-8819-875619ccb54d": {
                                    "uuid": "8a703cfa-450f-421a-8819-875619ccb54d",
                                    "label": "Yes",
                                    "advice": null,
                                    "metricMeasures": [],
                                    "followUpUuids": ["2e4307b9-93b8-4617-b8d1-ba0fa9f15e04"]
                                }
                            },
                            "experts": {},
                            "references": {},
                            "integrations": {},
                            "tags": {}
                        }
                        """

                    expected =
                        { chapters = Dict.empty
                        , questions = Dict.empty
                        , answers =
                            Dict.fromList
                                [ ( "8a703cfa-450f-421a-8819-875619ccb54d"
                                  , { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                                    , label = "Yes"
                                    , advice = Nothing
                                    , metricMeasures = []
                                    , followUpUuids = [ "2e4307b9-93b8-4617-b8d1-ba0fa9f15e04" ]
                                    }
                                  )
                                ]
                        , experts = Dict.empty
                        , references = Dict.empty
                        , integrations = Dict.empty
                        , tags = Dict.empty
                        }
                in
                expectDecoder KnowledgeModelEntities.decoder raw expected
        , test "should decode experts" <|
            \_ ->
                let
                    raw =
                        """
                        {
                            "chapters": {},
                            "questions": {},
                            "answers": {},
                            "experts": {
                                "8a703cfa-450f-421a-8819-875619ccb54d": {
                                    "uuid": "8a703cfa-450f-421a-8819-875619ccb54d",
                                    "name": "John Example",
                                    "email": "expert@example.com"
                                }
                            },
                            "references": {},
                            "integrations": {},
                            "tags": {}
                        }
                        """

                    expected =
                        { chapters = Dict.empty
                        , questions = Dict.empty
                        , answers = Dict.empty
                        , experts =
                            Dict.fromList
                                [ ( "8a703cfa-450f-421a-8819-875619ccb54d"
                                  , { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                                    , name = "John Example"
                                    , email = "expert@example.com"
                                    }
                                  )
                                ]
                        , references = Dict.empty
                        , integrations = Dict.empty
                        , tags = Dict.empty
                        }
                in
                expectDecoder KnowledgeModelEntities.decoder raw expected
        , test "should decode references" <|
            \_ ->
                let
                    raw =
                        """
                        {
                            "chapters": {},
                            "questions": {},
                            "answers": {},
                            "experts": {},
                            "references": {
                                "8a703cfa-450f-421a-8819-875619ccb54d": {
                                    "referenceType": "ResourcePageReference",
                                    "uuid": "8a703cfa-450f-421a-8819-875619ccb54d",
                                    "shortUuid": "atq"
                                }
                            },
                            "integrations": {},
                            "tags": {}
                        }
                        """

                    expected =
                        { chapters = Dict.empty
                        , questions = Dict.empty
                        , answers = Dict.empty
                        , experts = Dict.empty
                        , references =
                            Dict.fromList
                                [ ( "8a703cfa-450f-421a-8819-875619ccb54d"
                                  , ResourcePageReference
                                        { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                                        , shortUuid = "atq"
                                        }
                                  )
                                ]
                        , integrations = Dict.empty
                        , tags = Dict.empty
                        }
                in
                expectDecoder KnowledgeModelEntities.decoder raw expected
        , test "should decode integrations" <|
            \_ ->
                let
                    raw =
                        """
                        {
                            "chapters": {},
                            "questions": {},
                            "answers": {},
                            "experts": {},
                            "references": {},
                            "integrations": {
                                "8f831db8-6f7a-42bd-bcd6-7b5174fd1ec9": {
                                    "uuid": "8f831db8-6f7a-42bd-bcd6-7b5174fd1ec9",
                                    "id": "service",
                                    "name": "Service",
                                    "props": ["kind", "category"],
                                    "logo": "data:image/png;base64,...",
                                    "requestMethod": "GET",
                                    "requestUrl": "/",
                                    "requestHeaders": {"X_USER": "user"},
                                    "requestBody": "{}",
                                    "responseListField": "",
                                    "responseIdField": "id",
                                    "responseNameField": "title",
                                    "itemUrl": "http://example.com/${id}"
                                }
                            },
                            "tags": {}
                        }
                        """

                    expected =
                        { chapters = Dict.empty
                        , questions = Dict.empty
                        , answers = Dict.empty
                        , experts = Dict.empty
                        , references = Dict.empty
                        , integrations =
                            Dict.fromList
                                [ ( "8f831db8-6f7a-42bd-bcd6-7b5174fd1ec9"
                                  , { uuid = "8f831db8-6f7a-42bd-bcd6-7b5174fd1ec9"
                                    , id = "service"
                                    , name = "Service"
                                    , props = [ "kind", "category" ]
                                    , logo = "data:image/png;base64,..."
                                    , requestMethod = "GET"
                                    , requestUrl = "/"
                                    , requestHeaders = Dict.fromList [ ( "X_USER", "user" ) ]
                                    , requestBody = "{}"
                                    , responseListField = ""
                                    , responseIdField = "id"
                                    , responseNameField = "title"
                                    , itemUrl = "http://example.com/${id}"
                                    }
                                  )
                                ]
                        , tags = Dict.empty
                        }
                in
                expectDecoder KnowledgeModelEntities.decoder raw expected
        , test "should decode tags" <|
            \_ ->
                let
                    raw =
                        """
                        {
                            "chapters": {},
                            "questions": {},
                            "answers": {},
                            "experts": {},
                            "references": {},
                            "integrations": {},
                            "tags": {
                                "b5b6ed23-2afa-11e9-b210-d663bd873d93": {
                                    "uuid": "b5b6ed23-2afa-11e9-b210-d663bd873d93",
                                    "name": "Science",
                                    "description": null,
                                    "color": "#F5A623"
                                }
                            }
                        }
                        """

                    expected =
                        { chapters = Dict.empty
                        , questions = Dict.empty
                        , answers = Dict.empty
                        , experts = Dict.empty
                        , references = Dict.empty
                        , integrations = Dict.empty
                        , tags =
                            Dict.fromList
                                [ ( "b5b6ed23-2afa-11e9-b210-d663bd873d93"
                                  , { uuid = "b5b6ed23-2afa-11e9-b210-d663bd873d93"
                                    , name = "Science"
                                    , description = Nothing
                                    , color = "#F5A623"
                                    }
                                  )
                                ]
                        }
                in
                expectDecoder KnowledgeModelEntities.decoder raw expected
        ]
