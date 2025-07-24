module Wizard.Api.Models.KnowledgeModel.KnowledgeModelEntitiesTest exposing (knowledgeModelEntitiesDecoderTest)

import Dict
import Test exposing (Test, describe, test)
import TestUtils exposing (expectDecoder)
import Wizard.Api.Models.KnowledgeModel.Integration exposing (Integration(..))
import Wizard.Api.Models.KnowledgeModel.KnowledgeModelEntities as KnowledgeModelEntities
import Wizard.Api.Models.KnowledgeModel.Question exposing (Question(..))
import Wizard.Api.Models.KnowledgeModel.Reference exposing (Reference(..))


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
                            "choices": {},
                            "experts": {},
                            "references": {},
                            "integrations": {},
                            "tags": {},
                            "metrics": {},
                            "phases": {},
                            "resourceCollections": {},
                            "resourcePages": {}
                        }
                        """

                    expected =
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
                        , resourceCollections = Dict.empty
                        , resourcePages = Dict.empty
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
                                    "questionUuids": [],
                                    "annotations": []
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
                            "phases": {},
                            "resourceCollections": {},
                            "resourcePages": {}
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
                                    , annotations = []
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
                        , resourceCollections = Dict.empty
                        , resourcePages = Dict.empty
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
                                    "requiredPhaseUuid": null,
                                    "tagUuids": [],
                                    "referenceUuids": [],
                                    "expertUuids": [],
                                    "integrationUuid": "b50bf5ce-2fc3-4779-9756-5f176c233374",
                                    "variables": {
                                        "variable": "value"
                                    },
                                    "annotations": []
                                }
                            },
                            "answers": {},
                            "choices": {},
                            "experts": {},
                            "references": {},
                            "integrations": {},
                            "tags": {},
                            "metrics": {},
                            "phases": {},
                            "resourceCollections": {},
                            "resourcePages": {}
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
                                        , requiredPhaseUuid = Nothing
                                        , tagUuids = []
                                        , referenceUuids = []
                                        , expertUuids = []
                                        , annotations = []
                                        }
                                        { integrationUuid = "b50bf5ce-2fc3-4779-9756-5f176c233374"
                                        , variables = Dict.fromList [ ( "variable", "value" ) ]
                                        }
                                  )
                                ]
                        , answers = Dict.empty
                        , choices = Dict.empty
                        , experts = Dict.empty
                        , references = Dict.empty
                        , integrations = Dict.empty
                        , tags = Dict.empty
                        , metrics = Dict.empty
                        , phases = Dict.empty
                        , resourceCollections = Dict.empty
                        , resourcePages = Dict.empty
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
                                    "followUpUuids": ["2e4307b9-93b8-4617-b8d1-ba0fa9f15e04"],
                                    "annotations": []
                                }
                            },
                            "choices": {},
                            "experts": {},
                            "references": {},
                            "integrations": {},
                            "tags": {},
                            "metrics": {},
                            "phases": {},
                            "resourceCollections": {},
                            "resourcePages": {}
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
                                    , annotations = []
                                    }
                                  )
                                ]
                        , choices = Dict.empty
                        , experts = Dict.empty
                        , references = Dict.empty
                        , integrations = Dict.empty
                        , tags = Dict.empty
                        , metrics = Dict.empty
                        , phases = Dict.empty
                        , resourceCollections = Dict.empty
                        , resourcePages = Dict.empty
                        }
                in
                expectDecoder KnowledgeModelEntities.decoder raw expected
        , test "should decode choices" <|
            \_ ->
                let
                    raw =
                        """
                        {
                            "chapters": {},
                            "questions": {},
                            "answers": {},
                            "choices": {
                                "8a703cfa-450f-421a-8819-875619ccb54d": {
                                    "uuid": "8a703cfa-450f-421a-8819-875619ccb54d",
                                    "label": "Choice",
                                    "annotations": []
                                }
                            },
                            "experts": {},
                            "references": {},
                            "integrations": {},
                            "tags": {},
                            "metrics": {},
                            "phases": {},
                            "resourceCollections": {},
                            "resourcePages": {}
                        }
                        """

                    expected =
                        { chapters = Dict.empty
                        , questions = Dict.empty
                        , answers = Dict.empty
                        , choices =
                            Dict.fromList
                                [ ( "8a703cfa-450f-421a-8819-875619ccb54d"
                                  , { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                                    , label = "Choice"
                                    , annotations = []
                                    }
                                  )
                                ]
                        , experts = Dict.empty
                        , references = Dict.empty
                        , integrations = Dict.empty
                        , tags = Dict.empty
                        , metrics = Dict.empty
                        , phases = Dict.empty
                        , resourceCollections = Dict.empty
                        , resourcePages = Dict.empty
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
                            "choices": {},
                            "experts": {
                                "8a703cfa-450f-421a-8819-875619ccb54d": {
                                    "uuid": "8a703cfa-450f-421a-8819-875619ccb54d",
                                    "name": "John Example",
                                    "email": "expert@example.com",
                                    "annotations": []
                                }
                            },
                            "references": {},
                            "integrations": {},
                            "tags": {},
                            "metrics": {},
                            "phases": {},
                            "resourceCollections": {},
                            "resourcePages": {}
                        }
                        """

                    expected =
                        { chapters = Dict.empty
                        , questions = Dict.empty
                        , answers = Dict.empty
                        , choices = Dict.empty
                        , experts =
                            Dict.fromList
                                [ ( "8a703cfa-450f-421a-8819-875619ccb54d"
                                  , { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                                    , name = "John Example"
                                    , email = "expert@example.com"
                                    , annotations = []
                                    }
                                  )
                                ]
                        , references = Dict.empty
                        , integrations = Dict.empty
                        , tags = Dict.empty
                        , metrics = Dict.empty
                        , phases = Dict.empty
                        , resourceCollections = Dict.empty
                        , resourcePages = Dict.empty
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
                            "choices": {},
                            "experts": {},
                            "references": {
                                "8a703cfa-450f-421a-8819-875619ccb54d": {
                                    "referenceType": "ResourcePageReference",
                                    "uuid": "8a703cfa-450f-421a-8819-875619ccb54d",
                                    "resourcePageUuid": "ba931b74-6254-403e-a10e-ba14bd55e384",
                                    "annotations": []
                                }
                            },
                            "integrations": {},
                            "tags": {},
                            "metrics": {},
                            "phases": {},
                            "resourceCollections": {},
                            "resourcePages": {}
                        }
                        """

                    expected =
                        { chapters = Dict.empty
                        , questions = Dict.empty
                        , answers = Dict.empty
                        , choices = Dict.empty
                        , experts = Dict.empty
                        , references =
                            Dict.fromList
                                [ ( "8a703cfa-450f-421a-8819-875619ccb54d"
                                  , ResourcePageReference
                                        { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                                        , resourcePageUuid = Just "ba931b74-6254-403e-a10e-ba14bd55e384"
                                        , annotations = []
                                        }
                                  )
                                ]
                        , integrations = Dict.empty
                        , tags = Dict.empty
                        , metrics = Dict.empty
                        , phases = Dict.empty
                        , resourceCollections = Dict.empty
                        , resourcePages = Dict.empty
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
                            "choices": {},
                            "experts": {},
                            "references": {},
                            "integrations": {
                                "8f831db8-6f7a-42bd-bcd6-7b5174fd1ec9": {
                                    "integrationType": "ApiLegacyIntegration",
                                    "uuid": "8f831db8-6f7a-42bd-bcd6-7b5174fd1ec9",
                                    "id": "service",
                                    "name": "Service",
                                    "variables": ["kind", "category"],
                                    "logo": "data:image/png;base64,...",
                                    "itemUrl": "http://example.com/${id}",
                                    "requestMethod": "GET",
                                    "requestUrl": "/",
                                    "requestHeaders": [{"key": "X_USER", "value": "user"}],
                                    "requestBody": "{}",
                                    "requestEmptySearch": true,
                                    "responseListField": null,
                                    "responseItemId": "{{id}}",
                                    "responseItemTemplate": "{{title}}",
                                    "annotations": []
                                }
                            },
                            "tags": {},
                            "metrics": {},
                            "phases": {},
                            "resourceCollections": {},
                            "resourcePages": {}
                        }
                        """

                    expected =
                        { chapters = Dict.empty
                        , questions = Dict.empty
                        , answers = Dict.empty
                        , choices = Dict.empty
                        , experts = Dict.empty
                        , references = Dict.empty
                        , integrations =
                            Dict.fromList
                                [ ( "8f831db8-6f7a-42bd-bcd6-7b5174fd1ec9"
                                  , ApiLegacyIntegration
                                        { uuid = "8f831db8-6f7a-42bd-bcd6-7b5174fd1ec9"
                                        , id = "service"
                                        , name = "Service"
                                        , variables = [ "kind", "category" ]
                                        , logo = Just "data:image/png;base64,..."
                                        , itemUrl = Just "http://example.com/${id}"
                                        , annotations = []
                                        }
                                        { requestMethod = "GET"
                                        , requestUrl = "/"
                                        , requestHeaders = [ { key = "X_USER", value = "user" } ]
                                        , requestBody = "{}"
                                        , requestEmptySearch = True
                                        , responseListField = Nothing
                                        , responseItemId = Just "{{id}}"
                                        , responseItemTemplate = "{{title}}"
                                        }
                                  )
                                ]
                        , tags = Dict.empty
                        , metrics = Dict.empty
                        , phases = Dict.empty
                        , resourceCollections = Dict.empty
                        , resourcePages = Dict.empty
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
                            "choices": {},
                            "experts": {},
                            "references": {},
                            "integrations": {},
                            "tags": {
                                "b5b6ed23-2afa-11e9-b210-d663bd873d93": {
                                    "uuid": "b5b6ed23-2afa-11e9-b210-d663bd873d93",
                                    "name": "Science",
                                    "description": null,
                                    "color": "#F5A623",
                                    "annotations": []
                                }
                            },
                            "metrics": {},
                            "phases": {},
                            "resourceCollections": {},
                            "resourcePages": {}
                        }
                        """

                    expected =
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
                                    , annotations = []
                                    }
                                  )
                                ]
                        , metrics = Dict.empty
                        , phases = Dict.empty
                        , resourceCollections = Dict.empty
                        , resourcePages = Dict.empty
                        }
                in
                expectDecoder KnowledgeModelEntities.decoder raw expected
        ]
