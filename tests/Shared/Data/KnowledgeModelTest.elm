module Shared.Data.KnowledgeModelTest exposing (knowledgeModelDecoderTest)

import Dict
import Shared.Data.KnowledgeModel as KnowledgeModel
import Test exposing (..)
import TestUtils exposing (expectDecoder)
import Uuid


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
                            "chapterUuids": [],
                            "tagUuids": [],
                            "integrationUuids": [],
                            "metricUuids": [],
                            "phaseUuids": [],
                            "entities": {
                                "chapters": {},
                                "questions": {},
                                "answers": {},
                                "choices": {},
                                "experts": {},
                                "references": {},
                                "integrations": {},
                                "tags": {},
                                "metrics": {},
                                "phases": {}
                            }
                        }
                        """

                    expected =
                        { uuid = Uuid.fromUuidString "8a703cfa-450f-421a-8819-875619ccb54d"
                        , chapterUuids = []
                        , tagUuids = []
                        , integrationUuids = []
                        , metricUuids = []
                        , phaseUuids = []
                        , entities =
                            { chapters = Dict.empty
                            , questions = Dict.empty
                            , answers = Dict.empty
                            , choices = Dict.empty
                            , experts = Dict.empty
                            , references = Dict.empty
                            , integrations = Dict.empty
                            , tags = Dict.empty
                            , metrics = Dict.empty
                            , phases = Dict.empty
                            }
                        }
                in
                expectDecoder KnowledgeModel.decoder raw expected
        , test "should decode knowledge model with chapters" <|
            \_ ->
                let
                    raw =
                        """
                        {
                            "uuid": "8a703cfa-450f-421a-8819-875619ccb54d",
                            "chapterUuids": ["2e4307b9-93b8-4617-b8d1-ba0fa9f15e04"],
                            "tagUuids": [],
                            "integrationUuids": [],
                            "metricUuids": [],
                            "phaseUuids": [],
                            "entities": {
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
                                "choices": {},
                                "experts": {},
                                "references": {},
                                "integrations": {},
                                "tags": {},
                                "metrics": {},
                                "phases": {}
                            }
                        }
                        """

                    expected =
                        { uuid = Uuid.fromUuidString "8a703cfa-450f-421a-8819-875619ccb54d"
                        , chapterUuids = [ "2e4307b9-93b8-4617-b8d1-ba0fa9f15e04" ]
                        , tagUuids = []
                        , integrationUuids = []
                        , metricUuids = []
                        , phaseUuids = []
                        , entities =
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
                            , choices = Dict.empty
                            , experts = Dict.empty
                            , references = Dict.empty
                            , integrations = Dict.empty
                            , tags = Dict.empty
                            , metrics = Dict.empty
                            , phases = Dict.empty
                            }
                        }
                in
                expectDecoder KnowledgeModel.decoder raw expected
        , test "should decode knowledge model with tags" <|
            \_ ->
                let
                    raw =
                        """
                        {
                            "uuid": "8a703cfa-450f-421a-8819-875619ccb54d",
                            "chapterUuids": [],
                            "tagUuids": ["b5b6ed23-2afa-11e9-b210-d663bd873d93"],
                            "integrationUuids": [],
                            "metricUuids": [],
                            "phaseUuids": [],
                            "entities": {
                                "chapters": {},
                                "questions": {},
                                "answers": {},
                                "choices": {},
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
                                },
                                "metrics": {},
                                "phases": {}
                            }
                        }
                        """

                    expected =
                        { uuid = Uuid.fromUuidString "8a703cfa-450f-421a-8819-875619ccb54d"
                        , chapterUuids = []
                        , tagUuids = [ "b5b6ed23-2afa-11e9-b210-d663bd873d93" ]
                        , integrationUuids = []
                        , metricUuids = []
                        , phaseUuids = []
                        , entities =
                            { chapters = Dict.empty
                            , questions = Dict.empty
                            , answers = Dict.empty
                            , choices = Dict.empty
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
                            , metrics = Dict.empty
                            , phases = Dict.empty
                            }
                        }
                in
                expectDecoder KnowledgeModel.decoder raw expected
        , test "should decode knowledge model with integrations" <|
            \_ ->
                let
                    raw =
                        """
                        {
                            "uuid": "8a703cfa-450f-421a-8819-875619ccb54d",
                            "chapterUuids": [],
                            "tagUuids": [],
                            "integrationUuids": ["aae37504-aec6-4be8-b703-5bcb3502f3e6"],
                            "metricUuids": [],
                            "phaseUuids": [],
                            "entities": {
                                "chapters": {},
                                "questions": {},
                                "answers": {},
                                "choices": {},
                                "experts": {},
                                "references": {},
                                "integrations": {
                                    "aae37504-aec6-4be8-b703-5bcb3502f3e6": {
                                        "uuid": "aae37504-aec6-4be8-b703-5bcb3502f3e6",
                                        "id": "service",
                                        "name": "Service",
                                        "props": ["kind", "category"],
                                        "logo": "data:image/png;base64,...",
                                        "requestMethod": "GET",
                                        "requestUrl": "/",
                                        "requestHeaders": {"X_USER": "user"},
                                        "requestBody": "{}",
                                        "responseListField": "items",
                                        "responseIdField": "id",
                                        "responseNameField": "title",
                                        "itemUrl": "http://example.com/${id}"
                                    }
                                },
                                "tags": {},
                                "metrics": {},
                                "phases": {}
                            }
                        }
                        """

                    expected =
                        { uuid = Uuid.fromUuidString "8a703cfa-450f-421a-8819-875619ccb54d"
                        , chapterUuids = []
                        , tagUuids = []
                        , integrationUuids = [ "aae37504-aec6-4be8-b703-5bcb3502f3e6" ]
                        , metricUuids = []
                        , phaseUuids = []
                        , entities =
                            { chapters = Dict.empty
                            , questions = Dict.empty
                            , answers = Dict.empty
                            , choices = Dict.empty
                            , experts = Dict.empty
                            , references = Dict.empty
                            , integrations =
                                Dict.fromList
                                    [ ( "aae37504-aec6-4be8-b703-5bcb3502f3e6"
                                      , { uuid = "aae37504-aec6-4be8-b703-5bcb3502f3e6"
                                        , id = "service"
                                        , name = "Service"
                                        , props = [ "kind", "category" ]
                                        , logo = "data:image/png;base64,..."
                                        , requestMethod = "GET"
                                        , requestUrl = "/"
                                        , requestHeaders = Dict.fromList [ ( "X_USER", "user" ) ]
                                        , requestBody = "{}"
                                        , responseListField = "items"
                                        , responseIdField = "id"
                                        , responseNameField = "title"
                                        , itemUrl = "http://example.com/${id}"
                                        }
                                      )
                                    ]
                            , tags = Dict.empty
                            , metrics = Dict.empty
                            , phases = Dict.empty
                            }
                        }
                in
                expectDecoder KnowledgeModel.decoder raw expected
        , test "should decode knowledge model with metrics" <|
            \_ ->
                let
                    raw =
                        """
                        {
                            "uuid": "8a703cfa-450f-421a-8819-875619ccb54d",
                            "chapterUuids": [],
                            "tagUuids": [],
                            "integrationUuids": ["aae37504-aec6-4be8-b703-5bcb3502f3e6"],
                            "metricUuids": [],
                            "phaseUuids": [],
                            "entities": {
                                "chapters": {},
                                "questions": {},
                                "answers": {},
                                "choices": {},
                                "experts": {},
                                "references": {},
                                "integrations": {},
                                "tags": {},
                                "metrics": {
                                    "aae37504-aec6-4be8-b703-5bcb3502f3e6": {
                                        "uuid": "aae37504-aec6-4be8-b703-5bcb3502f3e6",
                                        "title": "Metric",
                                        "abbreviation": "M",
                                        "description": null
                                    }
                                },
                                "phases": {}
                            }
                        }
                        """

                    expected =
                        { uuid = Uuid.fromUuidString "8a703cfa-450f-421a-8819-875619ccb54d"
                        , chapterUuids = []
                        , tagUuids = []
                        , integrationUuids = [ "aae37504-aec6-4be8-b703-5bcb3502f3e6" ]
                        , metricUuids = []
                        , phaseUuids = []
                        , entities =
                            { chapters = Dict.empty
                            , questions = Dict.empty
                            , answers = Dict.empty
                            , choices = Dict.empty
                            , experts = Dict.empty
                            , references = Dict.empty
                            , integrations = Dict.empty
                            , tags = Dict.empty
                            , metrics =
                                Dict.fromList
                                    [ ( "aae37504-aec6-4be8-b703-5bcb3502f3e6"
                                      , { uuid = "aae37504-aec6-4be8-b703-5bcb3502f3e6"
                                        , title = "Metric"
                                        , abbreviation = Just "M"
                                        , description = Nothing
                                        }
                                      )
                                    ]
                            , phases = Dict.empty
                            }
                        }
                in
                expectDecoder KnowledgeModel.decoder raw expected
        , test "should decode knowledge model with phases" <|
            \_ ->
                let
                    raw =
                        """
                        {
                            "uuid": "8a703cfa-450f-421a-8819-875619ccb54d",
                            "chapterUuids": [],
                            "tagUuids": [],
                            "integrationUuids": ["aae37504-aec6-4be8-b703-5bcb3502f3e6"],
                            "metricUuids": [],
                            "phaseUuids": [],
                            "entities": {
                                "chapters": {},
                                "questions": {},
                                "answers": {},
                                "choices": {},
                                "experts": {},
                                "references": {},
                                "integrations": {},
                                "tags": {},
                                "metrics": {},
                                "phases": {
                                    "aae37504-aec6-4be8-b703-5bcb3502f3e6": {
                                        "uuid": "aae37504-aec6-4be8-b703-5bcb3502f3e6",
                                        "title": "Phase",
                                        "description": "This is a phase"
                                    }
                                }
                            }
                        }
                        """

                    expected =
                        { uuid = Uuid.fromUuidString "8a703cfa-450f-421a-8819-875619ccb54d"
                        , chapterUuids = []
                        , tagUuids = []
                        , integrationUuids = [ "aae37504-aec6-4be8-b703-5bcb3502f3e6" ]
                        , metricUuids = []
                        , phaseUuids = []
                        , entities =
                            { chapters = Dict.empty
                            , questions = Dict.empty
                            , answers = Dict.empty
                            , choices = Dict.empty
                            , experts = Dict.empty
                            , references = Dict.empty
                            , integrations = Dict.empty
                            , tags = Dict.empty
                            , metrics = Dict.empty
                            , phases =
                                Dict.fromList
                                    [ ( "aae37504-aec6-4be8-b703-5bcb3502f3e6"
                                      , { uuid = "aae37504-aec6-4be8-b703-5bcb3502f3e6"
                                        , title = "Phase"
                                        , description = Just "This is a phase"
                                        }
                                      )
                                    ]
                            }
                        }
                in
                expectDecoder KnowledgeModel.decoder raw expected
        ]
