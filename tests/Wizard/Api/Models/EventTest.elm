module Wizard.Api.Models.EventTest exposing
    ( addAnswerEventTest
    , addChapterEventTest
    , addChoiceEventTest
    , addExpertEventTest
    , addIntegrationEventTest
    , addMetricEventTest
    , addPhaseEventTest
    , addQuestionEventTest
    , addReferenceEventTest
    , addResourceCollectionEventTest
    , addResourcePageEventTest
    , addTagEventTest
    , deleteAnswerEventTest
    , deleteChapterEventTest
    , deleteChoiceEventTest
    , deleteExpertEventTest
    , deleteIntegrationEventTest
    , deleteMetricEventTest
    , deletePhaseEventTest
    , deleteQuestionEventTest
    , deleteReferenceEventTest
    , deleteResourceCollectionEventTest
    , deleteResourcePageEventTest
    , deleteTagEventTest
    , editAnswerEventTest
    , editChapterEventTest
    , editChoiceEventTest
    , editExpertEventTest
    , editIntegrationEventTest
    , editKnowledgeModelEventTest
    , editMetricEventTest
    , editPhaseEventTest
    , editQuestionEventTest
    , editReferenceEventTest
    , editResourceCollectionEventTest
    , editResourcePageEventTest
    , editTagEventTest
    , moveAnswerEventTest
    , moveChoiceEventTest
    , moveExpertEventTest
    , moveQuestionEventTest
    , moveReferenceEventTest
    )

import Dict
import Expect exposing (Expectation)
import Test exposing (Test, describe, test)
import TestUtils exposing (expectEncodeDecode, parametrized)
import Time
import Uuid
import Wizard.Api.Models.Event as Event exposing (Event(..))
import Wizard.Api.Models.Event.AddIntegrationEventData exposing (AddIntegrationEventData(..))
import Wizard.Api.Models.Event.AddQuestionEventData exposing (AddQuestionEventData(..))
import Wizard.Api.Models.Event.AddReferenceEventData exposing (AddReferenceEventData(..))
import Wizard.Api.Models.Event.EditIntegrationEventData exposing (EditIntegrationEventData(..))
import Wizard.Api.Models.Event.EditQuestionEventData exposing (EditQuestionEventData(..))
import Wizard.Api.Models.Event.EditReferenceEventData exposing (EditReferenceEventData(..))
import Wizard.Api.Models.KnowledgeModel.Question.QuestionValidation as QuestionValidation
import Wizard.Api.Models.KnowledgeModel.Question.QuestionValueType exposing (QuestionValueType(..))



{- knowledge model events -}


editKnowledgeModelEvent : Event
editKnowledgeModelEvent =
    EditKnowledgeModelEvent
        { chapterUuids =
            { changed = False
            , value = Nothing
            }
        , tagUuids =
            { changed = False
            , value = Nothing
            }
        , integrationUuids =
            { changed = False
            , value = Nothing
            }
        , metricUuids =
            { changed = False
            , value = Nothing
            }
        , phaseUuids =
            { changed = False
            , value = Nothing
            }
        , resourceCollectionUuids =
            { changed = False
            , value = Nothing
            }
        , annotations =
            { changed = False
            , value = Nothing
            }
        }
        { uuid = "79d1e7b4-c2d8-49ff-8293-dfcfdb6da6ac"
        , entityUuid = "aad436a7-c8a5-4237-a2bd-34decdf26a1f"
        , parentUuid = Uuid.toString Uuid.nil
        , createdAt = Time.millisToPosix 1642607898
        }


editKnowledgeModelEventTest : Test
editKnowledgeModelEventTest =
    describe "EditKnowledgeModel"
        [ test "should encode and decode" <|
            \_ ->
                expectEventEncodeDecode editKnowledgeModelEvent
        , test "get event uuid" <|
            \_ ->
                Expect.equal "79d1e7b4-c2d8-49ff-8293-dfcfdb6da6ac" (Event.getUuid editKnowledgeModelEvent)
        , test "get event entity visible name when not changed" <|
            \_ ->
                Expect.equal Nothing (Event.getEntityVisibleName editKnowledgeModelEvent)
        , test "get event entity visible name when changed" <|
            \_ ->
                Expect.equal Nothing (Event.getEntityVisibleName editKnowledgeModelEvent)
        ]



{- chapter events -}


addChapterEvent : Event
addChapterEvent =
    AddChapterEvent
        { title = "Design of Experiment"
        , text = Just "This is a chapter about the designing of the experiment"
        , annotations = []
        }
        { uuid = "485bc170-2df3-11e9-b210-d663bd873d93"
        , entityUuid = "42d0bd1e-2df3-11e9-b210-d663bd873d93"
        , parentUuid = "aad436a7-c8a5-4237-a2bd-34decdf26a1f"
        , createdAt = Time.millisToPosix 1642607898
        }


addChapterEventTest : Test
addChapterEventTest =
    describe "AddChapterEvent"
        [ test "should encode and decode" <|
            \_ ->
                expectEventEncodeDecode addChapterEvent
        , test "get event uuid" <|
            \_ ->
                Expect.equal "485bc170-2df3-11e9-b210-d663bd873d93" (Event.getUuid addChapterEvent)
        , test "get event entity visible name" <|
            \_ ->
                Expect.equal (Just "Design of Experiment") (Event.getEntityVisibleName addChapterEvent)
        ]


editChapterEvent : Event
editChapterEvent =
    EditChapterEvent
        { title =
            { changed = True
            , value = Just "Design of Experiment"
            }
        , text =
            { changed = False
            , value = Nothing
            }
        , questionUuids =
            { changed = True
            , value = Just [ "2877dc7e-2df6-11e9-b210-d663bd873d93", "2877df94-2df6-11e9-b210-d663bd873d93" ]
            }
        , annotations =
            { changed = False
            , value = Nothing
            }
        }
        { uuid = "485bc170-2df3-11e9-b210-d663bd873d93"
        , entityUuid = "42d0bd1e-2df3-11e9-b210-d663bd873d93"
        , parentUuid = "aad436a7-c8a5-4237-a2bd-34decdf26a1f"
        , createdAt = Time.millisToPosix 1642607898
        }


editChapterEventTest : Test
editChapterEventTest =
    describe "EditChapterEvent"
        [ test "should decode and encode" <|
            \_ ->
                expectEventEncodeDecode editChapterEvent
        , test "get event uuid" <|
            \_ ->
                Expect.equal "485bc170-2df3-11e9-b210-d663bd873d93" (Event.getUuid editChapterEvent)
        , test "get entity visible name when not changed" <|
            \_ ->
                let
                    event =
                        EditChapterEvent
                            { title =
                                { changed = False
                                , value = Nothing
                                }
                            , text =
                                { changed = False
                                , value = Nothing
                                }
                            , questionUuids =
                                { changed = True
                                , value = Just [ "2877dc7e-2df6-11e9-b210-d663bd873d93", "2877df94-2df6-11e9-b210-d663bd873d93" ]
                                }
                            , annotations =
                                { changed = False
                                , value = Nothing
                                }
                            }
                            { uuid = "485bc170-2df3-11e9-b210-d663bd873d93"
                            , entityUuid = "42d0bd1e-2df3-11e9-b210-d663bd873d93"
                            , parentUuid = "aad436a7-c8a5-4237-a2bd-34decdf26a1f"
                            , createdAt = Time.millisToPosix 1642607898
                            }
                in
                Expect.equal Nothing (Event.getEntityVisibleName event)
        , test "get entity visible name when changed" <|
            \_ ->
                Expect.equal (Just "Design of Experiment") (Event.getEntityVisibleName editChapterEvent)
        ]


deleteChapterEvent : Event
deleteChapterEvent =
    DeleteChapterEvent
        { uuid = "485bc170-2df3-11e9-b210-d663bd873d93"
        , entityUuid = "42d0bd1e-2df3-11e9-b210-d663bd873d93"
        , parentUuid = "aad436a7-c8a5-4237-a2bd-34decdf26a1f"
        , createdAt = Time.millisToPosix 1642607898
        }


deleteChapterEventTest : Test
deleteChapterEventTest =
    describe "DeleteChapterEvent"
        [ test "should encode and decode" <|
            \_ ->
                expectEventEncodeDecode deleteChapterEvent
        , test "get event uuid" <|
            \_ ->
                Expect.equal "485bc170-2df3-11e9-b210-d663bd873d93" (Event.getUuid deleteChapterEvent)
        , test "get entity visible name" <|
            \_ ->
                Expect.equal Nothing (Event.getEntityVisibleName deleteChapterEvent)
        ]



{- metric events -}


addMetricEvent : Event
addMetricEvent =
    AddMetricEvent
        { title = "Metric"
        , abbreviation = Just "M"
        , description = Nothing
        , annotations = []
        }
        { uuid = "485bc170-2df3-11e9-b210-d663bd873d93"
        , entityUuid = "1cf9c1f2-2df9-11e9-b210-d663bd873d93"
        , parentUuid = "aad436a7-c8a5-4237-a2bd-34decdf26a1f"
        , createdAt = Time.millisToPosix 1642607898
        }


addMetricEventTest : Test
addMetricEventTest =
    describe "AddMetricEvent"
        [ test "should encode and decode" <|
            \_ ->
                expectEventEncodeDecode addMetricEvent
        , test "get event uuid" <|
            \_ ->
                Expect.equal "485bc170-2df3-11e9-b210-d663bd873d93" (Event.getUuid addMetricEvent)
        , test "get event entity visible name" <|
            \_ ->
                Expect.equal (Just "Metric") (Event.getEntityVisibleName addMetricEvent)
        ]


editMetricEvent : Event
editMetricEvent =
    EditMetricEvent
        { title =
            { changed = True
            , value = Just "Metric"
            }
        , abbreviation =
            { changed = False
            , value = Nothing
            }
        , description =
            { changed = True
            , value = Just (Just "This is a metric")
            }
        , annotations =
            { changed = False
            , value = Nothing
            }
        }
        { uuid = "485bc170-2df3-11e9-b210-d663bd873d93"
        , entityUuid = "1cf9c1f2-2df9-11e9-b210-d663bd873d93"
        , parentUuid = "aad436a7-c8a5-4237-a2bd-34decdf26a1f"
        , createdAt = Time.millisToPosix 1642607898
        }


editMetricEventTest : Test
editMetricEventTest =
    describe "EditMetricEvent"
        [ test "should encode and decode" <|
            \_ -> expectEventEncodeDecode editMetricEvent
        , test "get event uuid" <|
            \_ ->
                Expect.equal "485bc170-2df3-11e9-b210-d663bd873d93" (Event.getUuid editMetricEvent)
        , test "get event entity visible name when not changed" <|
            \_ ->
                let
                    event =
                        EditMetricEvent
                            { title =
                                { changed = False
                                , value = Nothing
                                }
                            , abbreviation =
                                { changed = False
                                , value = Nothing
                                }
                            , description =
                                { changed = True
                                , value = Just (Just "This is a metric")
                                }
                            , annotations =
                                { changed = False
                                , value = Nothing
                                }
                            }
                            { uuid = "485bc170-2df3-11e9-b210-d663bd873d93"
                            , entityUuid = "1cf9c1f2-2df9-11e9-b210-d663bd873d93"
                            , parentUuid = "aad436a7-c8a5-4237-a2bd-34decdf26a1f"
                            , createdAt = Time.millisToPosix 1642607898
                            }
                in
                Expect.equal Nothing (Event.getEntityVisibleName event)
        , test "get event entity visible name when changed" <|
            \_ ->
                Expect.equal (Just "Metric") (Event.getEntityVisibleName editMetricEvent)
        ]


deleteMetricEvent : Event
deleteMetricEvent =
    DeleteMetricEvent
        { uuid = "485bc170-2df3-11e9-b210-d663bd873d93"
        , entityUuid = "1cf9c1f2-2df9-11e9-b210-d663bd873d93"
        , parentUuid = "aad436a7-c8a5-4237-a2bd-34decdf26a1f"
        , createdAt = Time.millisToPosix 1642607898
        }


deleteMetricEventTest : Test
deleteMetricEventTest =
    describe "DeleteMetricEvent"
        [ test "should encode and decode" <|
            \_ ->
                expectEventEncodeDecode deleteMetricEvent
        , test "get event uuid" <|
            \_ ->
                Expect.equal "485bc170-2df3-11e9-b210-d663bd873d93" (Event.getUuid deleteMetricEvent)
        , test "get entity visible name" <|
            \_ ->
                Expect.equal Nothing (Event.getEntityVisibleName deleteMetricEvent)
        ]



{- phase events -}


addPhaseEvent : Event
addPhaseEvent =
    AddPhaseEvent
        { title = "Phase"
        , description = Nothing
        , annotations = []
        }
        { uuid = "485bc170-2df3-11e9-b210-d663bd873d93"
        , entityUuid = "1cf9c1f2-2df9-11e9-b210-d663bd873d93"
        , parentUuid = "aad436a7-c8a5-4237-a2bd-34decdf26a1f"
        , createdAt = Time.millisToPosix 1642607898
        }


addPhaseEventTest : Test
addPhaseEventTest =
    describe "AddPhaseEvent"
        [ test "should encode and decode" <|
            \_ ->
                expectEventEncodeDecode addPhaseEvent
        , test "get event uuid" <|
            \_ ->
                Expect.equal "485bc170-2df3-11e9-b210-d663bd873d93" (Event.getUuid addPhaseEvent)
        , test "get event entity visible name" <|
            \_ ->
                Expect.equal (Just "Phase") (Event.getEntityVisibleName addPhaseEvent)
        ]


editPhaseEvent : Event
editPhaseEvent =
    EditPhaseEvent
        { title =
            { changed = True
            , value = Just "Phase"
            }
        , description =
            { changed = True
            , value = Just (Just "This is an important phase")
            }
        , annotations =
            { changed = False
            , value = Nothing
            }
        }
        { uuid = "485bc170-2df3-11e9-b210-d663bd873d93"
        , entityUuid = "1cf9c1f2-2df9-11e9-b210-d663bd873d93"
        , parentUuid = "aad436a7-c8a5-4237-a2bd-34decdf26a1f"
        , createdAt = Time.millisToPosix 1642607898
        }


editPhaseEventTest : Test
editPhaseEventTest =
    describe "EditPhaseEvent"
        [ test "should encode and decode" <|
            \_ -> expectEventEncodeDecode editPhaseEvent
        , test "get event uuid" <|
            \_ ->
                Expect.equal "485bc170-2df3-11e9-b210-d663bd873d93" (Event.getUuid editPhaseEvent)
        , test "get event entity visible name when not changed" <|
            \_ ->
                let
                    event =
                        EditPhaseEvent
                            { title =
                                { changed = False
                                , value = Nothing
                                }
                            , description =
                                { changed = True
                                , value = Just (Just "This is an important phase")
                                }
                            , annotations =
                                { changed = False
                                , value = Nothing
                                }
                            }
                            { uuid = "485bc170-2df3-11e9-b210-d663bd873d93"
                            , entityUuid = "1cf9c1f2-2df9-11e9-b210-d663bd873d93"
                            , parentUuid = "aad436a7-c8a5-4237-a2bd-34decdf26a1f"
                            , createdAt = Time.millisToPosix 1642607898
                            }
                in
                Expect.equal Nothing (Event.getEntityVisibleName event)
        , test "get event entity visible name when changed" <|
            \_ ->
                Expect.equal (Just "Phase") (Event.getEntityVisibleName editPhaseEvent)
        ]


deletePhaseEvent : Event
deletePhaseEvent =
    DeletePhaseEvent
        { uuid = "485bc170-2df3-11e9-b210-d663bd873d93"
        , entityUuid = "1cf9c1f2-2df9-11e9-b210-d663bd873d93"
        , parentUuid = "aad436a7-c8a5-4237-a2bd-34decdf26a1f"
        , createdAt = Time.millisToPosix 1642607898
        }


deletePhaseEventTest : Test
deletePhaseEventTest =
    describe "DeletePhaseEvent"
        [ test "should encode and decode" <|
            \_ ->
                expectEventEncodeDecode deletePhaseEvent
        , test "get event uuid" <|
            \_ ->
                Expect.equal "485bc170-2df3-11e9-b210-d663bd873d93" (Event.getUuid deletePhaseEvent)
        , test "get entity visible name" <|
            \_ ->
                Expect.equal Nothing (Event.getEntityVisibleName deletePhaseEvent)
        ]



{- tag events -}


addTagEvent : Event
addTagEvent =
    AddTagEvent
        { name = "Astronomy"
        , description = Just "Questions connected to astronomy"
        , color = "#F5A623"
        , annotations = []
        }
        { uuid = "485bc170-2df3-11e9-b210-d663bd873d93"
        , entityUuid = "1cf9c1f2-2df9-11e9-b210-d663bd873d93"
        , parentUuid = "aad436a7-c8a5-4237-a2bd-34decdf26a1f"
        , createdAt = Time.millisToPosix 1642607898
        }


addTagEventTest : Test
addTagEventTest =
    describe "AddTagEvent"
        [ test "should encode and decode" <|
            \_ ->
                expectEventEncodeDecode addTagEvent
        , test "get event uuid" <|
            \_ ->
                Expect.equal "485bc170-2df3-11e9-b210-d663bd873d93" (Event.getUuid addTagEvent)
        , test "get event entity visible name" <|
            \_ ->
                Expect.equal (Just "Astronomy") (Event.getEntityVisibleName addTagEvent)
        ]


editTagEvent : Event
editTagEvent =
    EditTagEvent
        { name =
            { changed = True
            , value = Just "Astronomy"
            }
        , description =
            { changed = False
            , value = Nothing
            }
        , color =
            { changed = True
            , value = Just "#F5A623"
            }
        , annotations =
            { changed = False
            , value = Nothing
            }
        }
        { uuid = "485bc170-2df3-11e9-b210-d663bd873d93"
        , entityUuid = "1cf9c1f2-2df9-11e9-b210-d663bd873d93"
        , parentUuid = "aad436a7-c8a5-4237-a2bd-34decdf26a1f"
        , createdAt = Time.millisToPosix 1642607898
        }


editTagEventTest : Test
editTagEventTest =
    describe "EditTagEvent"
        [ test "should encode and decode" <|
            \_ -> expectEventEncodeDecode editTagEvent
        , test "get event uuid" <|
            \_ ->
                Expect.equal "485bc170-2df3-11e9-b210-d663bd873d93" (Event.getUuid editTagEvent)
        , test "get event entity visible name when not changed" <|
            \_ ->
                let
                    event =
                        EditTagEvent
                            { name =
                                { changed = False
                                , value = Nothing
                                }
                            , description =
                                { changed = False
                                , value = Nothing
                                }
                            , color =
                                { changed = True
                                , value = Just "#F5A623"
                                }
                            , annotations =
                                { changed = False
                                , value = Nothing
                                }
                            }
                            { uuid = "485bc170-2df3-11e9-b210-d663bd873d93"
                            , entityUuid = "1cf9c1f2-2df9-11e9-b210-d663bd873d93"
                            , parentUuid = "aad436a7-c8a5-4237-a2bd-34decdf26a1f"
                            , createdAt = Time.millisToPosix 1642607898
                            }
                in
                Expect.equal Nothing (Event.getEntityVisibleName event)
        , test "get event entity visible name when changed" <|
            \_ ->
                Expect.equal (Just "Astronomy") (Event.getEntityVisibleName editTagEvent)
        ]


deleteTagEvent : Event
deleteTagEvent =
    DeleteTagEvent
        { uuid = "485bc170-2df3-11e9-b210-d663bd873d93"
        , entityUuid = "1cf9c1f2-2df9-11e9-b210-d663bd873d93"
        , parentUuid = "aad436a7-c8a5-4237-a2bd-34decdf26a1f"
        , createdAt = Time.millisToPosix 1642607898
        }


deleteTagEventTest : Test
deleteTagEventTest =
    describe "DeleteTagEvent"
        [ test "should encode and decode" <|
            \_ ->
                expectEventEncodeDecode deleteTagEvent
        , test "get event uuid" <|
            \_ ->
                Expect.equal "485bc170-2df3-11e9-b210-d663bd873d93" (Event.getUuid deleteTagEvent)
        , test "get entity visible name" <|
            \_ ->
                Expect.equal Nothing (Event.getEntityVisibleName deleteTagEvent)
        ]



{- integration events -}


addApiIntegrationEvent : Event
addApiIntegrationEvent =
    AddIntegrationEvent
        (AddIntegrationApiLegacyEvent
            { id = "service"
            , name = "Service"
            , variables = [ "kind", "category" ]
            , logo = Just "data:image/png;base64,..."
            , itemUrl = Just "http://example.com/${id}"
            , requestMethod = "GET"
            , requestUrl = "/api/search"
            , requestHeaders = [ { key = "X_USER", value = "user" } ]
            , requestBody = "{}"
            , requestEmptySearch = True
            , responseListField = Just "items"
            , responseItemId = Just "uuid"
            , responseItemTemplate = "title"
            , annotations = []
            }
        )
        { uuid = "cbecbad5-f85d-4e7e-95b9-34669e3333f9"
        , entityUuid = "0d03f237-bc95-4033-99ab-5ba3d85cd6c7"
        , parentUuid = "aad436a7-c8a5-4237-a2bd-34decdf26a1f"
        , createdAt = Time.millisToPosix 1642607898
        }


addWidgetIntegrationEvent : Event
addWidgetIntegrationEvent =
    AddIntegrationEvent
        (AddIntegrationWidgetEvent
            { id = "service"
            , name = "Service"
            , variables = [ "kind", "category" ]
            , logo = Just "data:image/png;base64,..."
            , itemUrl = Just "http://example.com/${id}"
            , widgetUrl = "http://example.com"
            , annotations = []
            }
        )
        { uuid = "cbecbad5-f85d-4e7e-95b9-34669e3333f9"
        , entityUuid = "0d03f237-bc95-4033-99ab-5ba3d85cd6c7"
        , parentUuid = "aad436a7-c8a5-4237-a2bd-34decdf26a1f"
        , createdAt = Time.millisToPosix 1642607898
        }


addIntegrationEventTest : Test
addIntegrationEventTest =
    describe "AddIntegrationEvent"
        [ parametrized
            [ addApiIntegrationEvent, addWidgetIntegrationEvent ]
            "should encode and decode"
          <|
            \event -> expectEventEncodeDecode event
        , parametrized
            [ addApiIntegrationEvent, addWidgetIntegrationEvent ]
            "get event uuid"
          <|
            \event ->
                Expect.equal "cbecbad5-f85d-4e7e-95b9-34669e3333f9" (Event.getUuid event)
        , parametrized
            [ addApiIntegrationEvent, addWidgetIntegrationEvent ]
            "get event entity visible name"
          <|
            \event ->
                Expect.equal (Just "Service") (Event.getEntityVisibleName event)
        ]


editApiIntegrationEvent : Event
editApiIntegrationEvent =
    EditIntegrationEvent
        (EditIntegrationApiLegacyEvent
            { id = { changed = True, value = Just "service" }
            , name = { changed = True, value = Just "Service" }
            , variables = { changed = True, value = Just [ "kind", "category" ] }
            , logo = { changed = False, value = Nothing }
            , itemUrl = { changed = True, value = Just (Just "http://example.com/${id}") }
            , requestMethod = { changed = True, value = Just "GET" }
            , requestUrl = { changed = False, value = Nothing }
            , requestHeaders = { changed = True, value = Just <| [ { key = "X_SEARCH", value = "full" }, { key = "X_USER", value = "user" } ] }
            , requestBody = { changed = True, value = Just "{}" }
            , requestEmptySearch = { changed = True, value = Just False }
            , responseListField = { changed = False, value = Nothing }
            , responseItemId = { changed = False, value = Nothing }
            , responseItemTemplate = { changed = True, value = Just "title" }
            , annotations = { changed = False, value = Nothing }
            }
        )
        { uuid = "cbecbad5-f85d-4e7e-95b9-34669e3333f9"
        , entityUuid = "52034933-3065-4876-9999-5f5c0d91f7aa"
        , parentUuid = "aad436a7-c8a5-4237-a2bd-34decdf26a1f"
        , createdAt = Time.millisToPosix 1642607898
        }


editWidgetIntegrationEvent : Event
editWidgetIntegrationEvent =
    EditIntegrationEvent
        (EditIntegrationWidgetEvent
            { id = { changed = True, value = Just "service" }
            , name = { changed = True, value = Just "Service" }
            , variables = { changed = True, value = Just [ "kind", "category" ] }
            , logo = { changed = False, value = Nothing }
            , itemUrl = { changed = True, value = Just (Just "http://example.com/${id}") }
            , widgetUrl = { changed = False, value = Nothing }
            , annotations = { changed = False, value = Nothing }
            }
        )
        { uuid = "cbecbad5-f85d-4e7e-95b9-34669e3333f9"
        , entityUuid = "52034933-3065-4876-9999-5f5c0d91f7aa"
        , parentUuid = "aad436a7-c8a5-4237-a2bd-34decdf26a1f"
        , createdAt = Time.millisToPosix 1642607898
        }


editIntegrationEventTest : Test
editIntegrationEventTest =
    describe "EditIntegrationEventTest"
        [ parametrized
            [ editApiIntegrationEvent, editWidgetIntegrationEvent ]
            "should encode and decode"
          <|
            \event -> expectEventEncodeDecode event
        , parametrized
            [ editApiIntegrationEvent, editWidgetIntegrationEvent ]
            "get event uuid"
          <|
            \event ->
                Expect.equal "cbecbad5-f85d-4e7e-95b9-34669e3333f9" (Event.getUuid event)
        , parametrized
            [ editApiIntegrationEvent, editWidgetIntegrationEvent ]
            "get event entity visible name"
          <|
            \event ->
                Expect.equal (Just "Service") (Event.getEntityVisibleName event)
        ]


deleteIntegrationEvent : Event
deleteIntegrationEvent =
    DeleteIntegrationEvent
        { uuid = "cbecbad5-f85d-4e7e-95b9-34669e3333f9"
        , entityUuid = "52034933-3065-4876-9999-5f5c0d91f7aa"
        , parentUuid = "aad436a7-c8a5-4237-a2bd-34decdf26a1f"
        , createdAt = Time.millisToPosix 1642607898
        }


deleteIntegrationEventTest : Test
deleteIntegrationEventTest =
    describe "DeleteIntegrationEvent"
        [ test "should encode and decode" <|
            \_ -> expectEventEncodeDecode deleteIntegrationEvent
        , test "get event uuid" <|
            \_ ->
                Expect.equal "cbecbad5-f85d-4e7e-95b9-34669e3333f9" (Event.getUuid deleteIntegrationEvent)
        , test "get event entity visible name" <|
            \_ ->
                Expect.equal Nothing (Event.getEntityVisibleName deleteIntegrationEvent)
        ]



{- question events -}


addOptionsQuestionEvent : Event
addOptionsQuestionEvent =
    AddQuestionEvent
        (AddQuestionOptionsEvent
            { title = "Can you answer this question?"
            , text = Nothing
            , requiredPhaseUuid = Just "0948bd26-d985-4549-b7c8-95e9061d6413"
            , tagUuids = []
            , annotations = []
            }
        )
        { uuid = "b09ed98c-3043-11e9-b210-d663bd873d93"
        , entityUuid = "a5405e3a-3043-11e9-b210-d663bd873d93"
        , parentUuid = "42d0bd1e-2df3-11e9-b210-d663bd873d93"
        , createdAt = Time.millisToPosix 1642607898
        }


addListQuestionEvent : Event
addListQuestionEvent =
    AddQuestionEvent
        (AddQuestionListEvent
            { title = "Can you answer this question?"
            , text = Just "Just answer the question!"
            , requiredPhaseUuid = Just "0948bd26-d985-4549-b7c8-95e9061d6413"
            , tagUuids = []
            , annotations = []
            }
        )
        { uuid = "b09ed98c-3043-11e9-b210-d663bd873d93"
        , entityUuid = "a5405e3a-3043-11e9-b210-d663bd873d93"
        , parentUuid = "42d0bd1e-2df3-11e9-b210-d663bd873d93"
        , createdAt = Time.millisToPosix 1642607898
        }


addValueQuestionEvent : Event
addValueQuestionEvent =
    AddQuestionEvent
        (AddQuestionValueEvent
            { title = "Can you answer this question?"
            , text = Nothing
            , requiredPhaseUuid = Nothing
            , tagUuids = [ "dc1dcc8a-3043-11e9-b210-d663bd873d93", "dc1dcf00-3043-11e9-b210-d663bd873d93" ]
            , valueType = NumberQuestionValueType
            , validations = []
            , annotations = []
            }
        )
        { uuid = "b09ed98c-3043-11e9-b210-d663bd873d93"
        , entityUuid = "a5405e3a-3043-11e9-b210-d663bd873d93"
        , parentUuid = "42d0bd1e-2df3-11e9-b210-d663bd873d93"
        , createdAt = Time.millisToPosix 1642607898
        }


addIntegrationQuestionEvent : Event
addIntegrationQuestionEvent =
    AddQuestionEvent
        (AddQuestionIntegrationEvent
            { title = "Can you answer this question?"
            , text = Nothing
            , requiredPhaseUuid = Nothing
            , tagUuids = [ "dc1dcc8a-3043-11e9-b210-d663bd873d93", "dc1dcf00-3043-11e9-b210-d663bd873d93" ]
            , integrationUuid = "1d522339-e93b-44e9-bc2a-1df65fb97dc6"
            , variables = Dict.fromList [ ( "prop1", "value1" ), ( "prop2", "value2" ) ]
            , annotations = []
            }
        )
        { uuid = "b09ed98c-3043-11e9-b210-d663bd873d93"
        , entityUuid = "a5405e3a-3043-11e9-b210-d663bd873d93"
        , parentUuid = "42d0bd1e-2df3-11e9-b210-d663bd873d93"
        , createdAt = Time.millisToPosix 1642607898
        }


addQuestionEventTest : Test
addQuestionEventTest =
    describe "AddQuestionEvent"
        [ parametrized
            [ addOptionsQuestionEvent, addListQuestionEvent, addValueQuestionEvent, addIntegrationQuestionEvent ]
            "should encode decode"
          <|
            \event ->
                expectEventEncodeDecode event
        , parametrized
            [ addOptionsQuestionEvent, addListQuestionEvent, addValueQuestionEvent, addIntegrationQuestionEvent ]
            "get event uuid"
          <|
            \event ->
                Expect.equal "b09ed98c-3043-11e9-b210-d663bd873d93" (Event.getUuid event)
        , parametrized
            [ addOptionsQuestionEvent, addListQuestionEvent, addValueQuestionEvent, addIntegrationQuestionEvent ]
            "get event entity visible name"
          <|
            \event ->
                Expect.equal (Just "Can you answer this question?") (Event.getEntityVisibleName event)
        ]


editOptionsQuestionEvent : Event
editOptionsQuestionEvent =
    EditQuestionEvent
        (EditQuestionOptionsEvent
            { title = { changed = False, value = Nothing }
            , text = { changed = True, value = Just (Just "Answer this immediately") }
            , requiredPhaseUuid = { changed = True, value = Just (Just "0948bd26-d985-4549-b7c8-95e9061d6413") }
            , tagUuids = { changed = False, value = Nothing }
            , referenceUuids = { changed = False, value = Nothing }
            , expertUuids = { changed = True, value = Just [ "fe1b440e-3046-11e9-b210-d663bd873d93" ] }
            , answerUuids = { changed = True, value = Just [ "5cb0bedc-3046-11e9-b210-d663bd873d93", "5cb0c15c-3046-11e9-b210-d663bd873d93" ] }
            , annotations = { changed = False, value = Nothing }
            }
        )
        { uuid = "b09ed98c-3043-11e9-b210-d663bd873d93"
        , entityUuid = "a5405e3a-3043-11e9-b210-d663bd873d93"
        , parentUuid = "42d0bd1e-2df3-11e9-b210-d663bd873d93"
        , createdAt = Time.millisToPosix 1642607898
        }


editListQuestionEvent : Event
editListQuestionEvent =
    EditQuestionEvent
        (EditQuestionListEvent
            { title = { changed = True, value = Just "This is a new title" }
            , text = { changed = False, value = Nothing }
            , requiredPhaseUuid = { changed = False, value = Nothing }
            , tagUuids = { changed = False, value = Nothing }
            , referenceUuids = { changed = True, value = Just [ "f749367c-3046-11e9-b210-d663bd873d93" ] }
            , expertUuids = { changed = False, value = Nothing }
            , itemTemplateQuestionUuids = { changed = True, value = Just [ "b2c867fc-3046-11e9-b210-d663bd873d93" ] }
            , annotations = { changed = False, value = Nothing }
            }
        )
        { uuid = "b09ed98c-3043-11e9-b210-d663bd873d93"
        , entityUuid = "a5405e3a-3043-11e9-b210-d663bd873d93"
        , parentUuid = "42d0bd1e-2df3-11e9-b210-d663bd873d93"
        , createdAt = Time.millisToPosix 1642607898
        }


editValueQuestionEvent : Event
editValueQuestionEvent =
    EditQuestionEvent
        (EditQuestionValueEvent
            { title = { changed = True, value = Just "What date is today?" }
            , text = { changed = False, value = Nothing }
            , requiredPhaseUuid = { changed = True, value = Just (Just "0948bd26-d985-4549-b7c8-95e9061d6413") }
            , tagUuids = { changed = True, value = Just [ "e734907e-3046-11e9-b210-d663bd873d93", "e73495ce-3046-11e9-b210-d663bd873d93", "e7349740-3046-11e9-b210-d663bd873d93" ] }
            , referenceUuids = { changed = False, value = Nothing }
            , expertUuids = { changed = False, value = Nothing }
            , valueType = { changed = True, value = Just DateQuestionValueType }
            , validations = { changed = True, value = Just [ QuestionValidation.FromDate { value = "2024-11-19" } ] }
            , annotations = { changed = False, value = Nothing }
            }
        )
        { uuid = "b09ed98c-3043-11e9-b210-d663bd873d93"
        , entityUuid = "a5405e3a-3043-11e9-b210-d663bd873d93"
        , parentUuid = "42d0bd1e-2df3-11e9-b210-d663bd873d93"
        , createdAt = Time.millisToPosix 1642607898
        }


editIntegrationQuestionEvent : Event
editIntegrationQuestionEvent =
    EditQuestionEvent
        (EditQuestionIntegrationEvent
            { title = { changed = True, value = Just "What database will you use?" }
            , text = { changed = False, value = Nothing }
            , requiredPhaseUuid = { changed = True, value = Just (Just "0948bd26-d985-4549-b7c8-95e9061d6413") }
            , tagUuids = { changed = True, value = Just [ "e734907e-3046-11e9-b210-d663bd873d93", "e73495ce-3046-11e9-b210-d663bd873d93", "e7349740-3046-11e9-b210-d663bd873d93" ] }
            , referenceUuids = { changed = False, value = Nothing }
            , expertUuids = { changed = False, value = Nothing }
            , integrationUuid = { changed = True, value = Just "1d522339-e93b-44e9-bc2a-1df65fb97dc6" }
            , variables = { changed = True, value = Just <| Dict.fromList [ ( "prop1", "value1" ), ( "prop2", "value2" ) ] }
            , annotations = { changed = False, value = Nothing }
            }
        )
        { uuid = "b09ed98c-3043-11e9-b210-d663bd873d93"
        , entityUuid = "a5405e3a-3043-11e9-b210-d663bd873d93"
        , parentUuid = "42d0bd1e-2df3-11e9-b210-d663bd873d93"
        , createdAt = Time.millisToPosix 1642607898
        }


editQuestionEventTest : Test
editQuestionEventTest =
    describe "EditQuestionEvent"
        [ parametrized
            [ editOptionsQuestionEvent, editListQuestionEvent, editValueQuestionEvent, editIntegrationQuestionEvent ]
            "should encode decode"
          <|
            \event ->
                expectEventEncodeDecode event
        , parametrized
            [ editOptionsQuestionEvent, editListQuestionEvent, editValueQuestionEvent, editIntegrationQuestionEvent ]
            "get event uuid"
          <|
            \event ->
                Expect.equal "b09ed98c-3043-11e9-b210-d663bd873d93" (Event.getUuid event)
        , parametrized
            [ ( editOptionsQuestionEvent, Nothing )
            , ( editListQuestionEvent, Just "This is a new title" )
            , ( editValueQuestionEvent, Just "What date is today?" )
            , ( editIntegrationQuestionEvent, Just "What database will you use?" )
            ]
            "get event entity visible name"
          <|
            \( event, name ) ->
                Expect.equal name (Event.getEntityVisibleName event)
        ]


deleteQuestionEvent : Event
deleteQuestionEvent =
    DeleteQuestionEvent
        { uuid = "b09ed98c-3043-11e9-b210-d663bd873d93"
        , entityUuid = "a5405e3a-3043-11e9-b210-d663bd873d93"
        , parentUuid = "42d0bd1e-2df3-11e9-b210-d663bd873d93"
        , createdAt = Time.millisToPosix 1642607898
        }


deleteQuestionEventTest : Test
deleteQuestionEventTest =
    describe "DeleteQuestionEvent"
        [ test "should encode decode" <|
            \_ ->
                expectEventEncodeDecode deleteQuestionEvent
        , test "get event uuid" <|
            \_ ->
                Expect.equal "b09ed98c-3043-11e9-b210-d663bd873d93" (Event.getUuid deleteQuestionEvent)
        , test "get entity visible name" <|
            \_ ->
                Expect.equal Nothing (Event.getEntityVisibleName deleteQuestionEvent)
        ]


moveQuestionEvent : Event
moveQuestionEvent =
    MoveQuestionEvent
        { targetUuid = "71268f6c-04bd-4d83-9418-318c619e7444" }
        { uuid = "349624f6-2dfc-11e9-b210-d663bd873d93"
        , entityUuid = "bad22d1c-2e01-11e9-b210-d663bd873d93"
        , parentUuid = "2f73c924-2dfc-11e9-b210-d663bd873d93"
        , createdAt = Time.millisToPosix 1642607898
        }


moveQuestionEventTest : Test
moveQuestionEventTest =
    describe "MoveQuestionEvent"
        [ test "should encode and decode" <|
            \_ -> expectEventEncodeDecode moveQuestionEvent
        , test "get event uuid" <|
            \_ ->
                Expect.equal "349624f6-2dfc-11e9-b210-d663bd873d93" (Event.getUuid moveQuestionEvent)
        , test "get event entity visible name" <|
            \_ ->
                Expect.equal Nothing (Event.getEntityVisibleName moveQuestionEvent)
        ]



{- answer events -}


addAnswerEvent : Event
addAnswerEvent =
    AddAnswerEvent
        { label = "Yes"
        , advice = Just "Good choice"
        , metricMeasures =
            [ { metricUuid = "1ca4da0a-2e00-11e9-b210-d663bd873d93"
              , measure = 0.5
              , weight = 1
              }
            ]
        , annotations = []
        }
        { uuid = "efac9f6e-2e00-11e9-b210-d663bd873d93"
        , entityUuid = "2bbe5372-2e00-11e9-b210-d663bd873d93"
        , parentUuid = "2f73c924-2dfc-11e9-b210-d663bd873d93"
        , createdAt = Time.millisToPosix 1642607898
        }


addAnswerEventTest : Test
addAnswerEventTest =
    describe "AddAnswerEvent"
        [ test "should encode and decode" <|
            \_ ->
                expectEventEncodeDecode addAnswerEvent
        , test "get event uuid" <|
            \_ ->
                Expect.equal "efac9f6e-2e00-11e9-b210-d663bd873d93" (Event.getUuid addAnswerEvent)
        , test "get event entity visible name" <|
            \_ ->
                Expect.equal (Just "Yes") (Event.getEntityVisibleName addAnswerEvent)
        ]


editAnswerEvent : Event
editAnswerEvent =
    EditAnswerEvent
        { label =
            { changed = True
            , value = Just "No"
            }
        , advice =
            { changed = False
            , value = Nothing
            }
        , metricMeasures =
            { changed = False
            , value = Nothing
            }
        , followUpUuids =
            { changed = True
            , value =
                Just
                    [ "734afa38-2e00-11e9-b210-d663bd873d93"
                    , "734afd12-2e00-11e9-b210-d663bd873d93"
                    , "734b005a-2e00-11e9-b210-d663bd873d93"
                    ]
            }
        , annotations =
            { changed = False
            , value = Nothing
            }
        }
        { uuid = "efac9f6e-2e00-11e9-b210-d663bd873d93"
        , entityUuid = "2bbe5372-2e00-11e9-b210-d663bd873d93"
        , parentUuid = "2f73c924-2dfc-11e9-b210-d663bd873d93"
        , createdAt = Time.millisToPosix 1642607898
        }


editAnswerEventTest : Test
editAnswerEventTest =
    describe "EditAnswerEvent"
        [ test "should decode and encode" <|
            \_ ->
                expectEventEncodeDecode editAnswerEvent
        , test "get event uuid" <|
            \_ ->
                Expect.equal "efac9f6e-2e00-11e9-b210-d663bd873d93" (Event.getUuid editAnswerEvent)
        , test "get entity visible name when not changed" <|
            \_ ->
                let
                    event =
                        EditAnswerEvent
                            { label =
                                { changed = False
                                , value = Nothing
                                }
                            , advice =
                                { changed = False
                                , value = Nothing
                                }
                            , metricMeasures =
                                { changed = False
                                , value = Nothing
                                }
                            , followUpUuids =
                                { changed = True
                                , value =
                                    Just
                                        [ "734afa38-2e00-11e9-b210-d663bd873d93"
                                        , "734afd12-2e00-11e9-b210-d663bd873d93"
                                        , "734b005a-2e00-11e9-b210-d663bd873d93"
                                        ]
                                }
                            , annotations =
                                { changed = False
                                , value = Nothing
                                }
                            }
                            { uuid = "efac9f6e-2e00-11e9-b210-d663bd873d93"
                            , entityUuid = "2bbe5372-2e00-11e9-b210-d663bd873d93"
                            , parentUuid = "2f73c924-2dfc-11e9-b210-d663bd873d93"
                            , createdAt = Time.millisToPosix 1642607898
                            }
                in
                Expect.equal Nothing (Event.getEntityVisibleName event)
        , test "get entity visible name when changed" <|
            \_ ->
                Expect.equal (Just "No") (Event.getEntityVisibleName editAnswerEvent)
        ]


deleteAnswerEvent : Event
deleteAnswerEvent =
    DeleteAnswerEvent
        { uuid = "efac9f6e-2e00-11e9-b210-d663bd873d93"
        , entityUuid = "2bbe5372-2e00-11e9-b210-d663bd873d93"
        , parentUuid = "2f73c924-2dfc-11e9-b210-d663bd873d93"
        , createdAt = Time.millisToPosix 1642607898
        }


deleteAnswerEventTest : Test
deleteAnswerEventTest =
    describe "DeleteAnswerEvent"
        [ test "should encode and decode" <|
            \_ ->
                expectEventEncodeDecode deleteAnswerEvent
        , test "get event uuid" <|
            \_ ->
                Expect.equal "efac9f6e-2e00-11e9-b210-d663bd873d93" (Event.getUuid deleteAnswerEvent)
        , test "get entity visible name" <|
            \_ ->
                Expect.equal Nothing (Event.getEntityVisibleName deleteAnswerEvent)
        ]


moveAnswerEvent : Event
moveAnswerEvent =
    MoveAnswerEvent
        { targetUuid = "71268f6c-04bd-4d83-9418-318c619e7444" }
        { uuid = "349624f6-2dfc-11e9-b210-d663bd873d93"
        , entityUuid = "bad22d1c-2e01-11e9-b210-d663bd873d93"
        , parentUuid = "2f73c924-2dfc-11e9-b210-d663bd873d93"
        , createdAt = Time.millisToPosix 1642607898
        }


moveAnswerEventTest : Test
moveAnswerEventTest =
    describe "MoveAnswerEvent"
        [ test "should encode and decode" <|
            \_ -> expectEventEncodeDecode moveAnswerEvent
        , test "get event uuid" <|
            \_ ->
                Expect.equal "349624f6-2dfc-11e9-b210-d663bd873d93" (Event.getUuid moveAnswerEvent)
        , test "get event entity visible name" <|
            \_ ->
                Expect.equal Nothing (Event.getEntityVisibleName moveAnswerEvent)
        ]



{- choice events -}


addChoiceEvent : Event
addChoiceEvent =
    AddChoiceEvent
        { label = "Choice"
        , annotations = []
        }
        { uuid = "349624f6-2dfc-11e9-b210-d663bd873d93"
        , entityUuid = "bad22d1c-2e01-11e9-b210-d663bd873d93"
        , parentUuid = "2f73c924-2dfc-11e9-b210-d663bd873d93"
        , createdAt = Time.millisToPosix 1642607898
        }


addChoiceEventTest : Test
addChoiceEventTest =
    describe "AddChoiceEvent"
        [ test "should encode and decode" <|
            \_ -> expectEventEncodeDecode addChoiceEvent
        , test "get event uuid" <|
            \_ ->
                Expect.equal "349624f6-2dfc-11e9-b210-d663bd873d93" (Event.getUuid addChoiceEvent)
        , test "get event entity visible name" <|
            \_ ->
                Expect.equal (Just "Choice") (Event.getEntityVisibleName addChoiceEvent)
        ]


editChoiceEvent : Event
editChoiceEvent =
    EditChoiceEvent
        { label =
            { changed = True
            , value = Just "New Choice"
            }
        , annotations =
            { changed = False
            , value = Nothing
            }
        }
        { uuid = "349624f6-2dfc-11e9-b210-d663bd873d93"
        , entityUuid = "bad22d1c-2e01-11e9-b210-d663bd873d93"
        , parentUuid = "2f73c924-2dfc-11e9-b210-d663bd873d93"
        , createdAt = Time.millisToPosix 1642607898
        }


editChoiceEventTest : Test
editChoiceEventTest =
    describe "EditChoiceEvent"
        [ test "should encode and decode" <|
            \_ -> expectEventEncodeDecode editChoiceEvent
        , test "get event uuid" <|
            \_ ->
                Expect.equal "349624f6-2dfc-11e9-b210-d663bd873d93" (Event.getUuid editChoiceEvent)
        , test "get event entity visible name when not changed" <|
            \_ ->
                let
                    event =
                        EditChoiceEvent
                            { label =
                                { changed = False
                                , value = Nothing
                                }
                            , annotations =
                                { changed = False
                                , value = Nothing
                                }
                            }
                            { uuid = "349624f6-2dfc-11e9-b210-d663bd873d93"
                            , entityUuid = "bad22d1c-2e01-11e9-b210-d663bd873d93"
                            , parentUuid = "2f73c924-2dfc-11e9-b210-d663bd873d93"
                            , createdAt = Time.millisToPosix 1642607898
                            }
                in
                Expect.equal Nothing (Event.getEntityVisibleName event)
        , test "get event entity visible name when changed" <|
            \_ ->
                Expect.equal (Just "New Choice") (Event.getEntityVisibleName editChoiceEvent)
        ]


deleteChoiceEvent : Event
deleteChoiceEvent =
    DeleteChoiceEvent
        { uuid = "349624f6-2dfc-11e9-b210-d663bd873d93"
        , entityUuid = "bad22d1c-2e01-11e9-b210-d663bd873d93"
        , parentUuid = "2f73c924-2dfc-11e9-b210-d663bd873d93"
        , createdAt = Time.millisToPosix 1642607898
        }


deleteChoiceEventTest : Test
deleteChoiceEventTest =
    describe "DeleteChoiceEvent"
        [ test "should encode and decode" <|
            \_ ->
                expectEventEncodeDecode deleteChoiceEvent
        , test "get event uuid" <|
            \_ ->
                Expect.equal "349624f6-2dfc-11e9-b210-d663bd873d93" (Event.getUuid deleteChoiceEvent)
        , test "get entity visible name" <|
            \_ ->
                Expect.equal Nothing (Event.getEntityVisibleName deleteChoiceEvent)
        ]


moveChoiceEvent : Event
moveChoiceEvent =
    MoveChoiceEvent
        { targetUuid = "71268f6c-04bd-4d83-9418-318c619e7444" }
        { uuid = "349624f6-2dfc-11e9-b210-d663bd873d93"
        , entityUuid = "bad22d1c-2e01-11e9-b210-d663bd873d93"
        , parentUuid = "2f73c924-2dfc-11e9-b210-d663bd873d93"
        , createdAt = Time.millisToPosix 1642607898
        }


moveChoiceEventTest : Test
moveChoiceEventTest =
    describe "MoveChoiceEvent"
        [ test "should encode and decode" <|
            \_ -> expectEventEncodeDecode moveChoiceEvent
        , test "get event uuid" <|
            \_ ->
                Expect.equal "349624f6-2dfc-11e9-b210-d663bd873d93" (Event.getUuid moveChoiceEvent)
        , test "get event entity visible name" <|
            \_ ->
                Expect.equal Nothing (Event.getEntityVisibleName moveChoiceEvent)
        ]



{- reference events -}


addResourcePageReferenceEvent : Event
addResourcePageReferenceEvent =
    AddReferenceEvent
        (AddReferenceResourcePageEvent
            { resourcePageUuid = Just "ba931b74-6254-403e-a10e-ba14bd55e384"
            , annotations = []
            }
        )
        { uuid = "349624f6-2dfc-11e9-b210-d663bd873d93"
        , entityUuid = "3f52e8fc-2dfc-11e9-b210-d663bd873d93"
        , parentUuid = "2f73c924-2dfc-11e9-b210-d663bd873d93"
        , createdAt = Time.millisToPosix 1642607898
        }


addURLReferenceEvent : Event
addURLReferenceEvent =
    AddReferenceEvent
        (AddReferenceURLEvent
            { url = "http://example.com"
            , label = "Example"
            , annotations = []
            }
        )
        { uuid = "349624f6-2dfc-11e9-b210-d663bd873d93"
        , entityUuid = "e559cf36-2dfc-11e9-b210-d663bd873d93"
        , parentUuid = "2f73c924-2dfc-11e9-b210-d663bd873d93"
        , createdAt = Time.millisToPosix 1642607898
        }


addCrossReferenceEvent : Event
addCrossReferenceEvent =
    AddReferenceEvent
        (AddReferenceCrossEvent
            { targetUuid = "072af95a-2dfd-11e9-b210-d663bd873d93"
            , description = "Related"
            , annotations = []
            }
        )
        { uuid = "349624f6-2dfc-11e9-b210-d663bd873d93"
        , entityUuid = "fe19113a-2dfc-11e9-b210-d663bd873d93"
        , parentUuid = "2f73c924-2dfc-11e9-b210-d663bd873d93"
        , createdAt = Time.millisToPosix 1642607898
        }


addReferenceEventTest : Test
addReferenceEventTest =
    describe "AddReferenceEvent"
        [ parametrized
            [ addResourcePageReferenceEvent, addURLReferenceEvent, addCrossReferenceEvent ]
            "should encode decode"
          <|
            \event ->
                expectEventEncodeDecode event
        , parametrized
            [ addResourcePageReferenceEvent, addURLReferenceEvent, addCrossReferenceEvent ]
            "get event uuid"
          <|
            \event ->
                Expect.equal "349624f6-2dfc-11e9-b210-d663bd873d93" (Event.getUuid event)
        , parametrized
            [ ( addResourcePageReferenceEvent, "ba931b74-6254-403e-a10e-ba14bd55e384" )
            , ( addURLReferenceEvent, "Example" )
            , ( addCrossReferenceEvent, "072af95a-2dfd-11e9-b210-d663bd873d93" )
            ]
            "get event entity visible name"
          <|
            \( event, name ) ->
                Expect.equal (Just name) (Event.getEntityVisibleName event)
        ]


editResourcePageReferenceEvent : Event
editResourcePageReferenceEvent =
    EditReferenceEvent
        (EditReferenceResourcePageEvent
            { resourcePageUuid =
                { changed = True
                , value = Just (Just "ba931b74-6254-403e-a10e-ba14bd55e384")
                }
            , annotations =
                { changed = False
                , value = Nothing
                }
            }
        )
        { uuid = "349624f6-2dfc-11e9-b210-d663bd873d93"
        , entityUuid = "3f52e8fc-2dfc-11e9-b210-d663bd873d93"
        , parentUuid = "2f73c924-2dfc-11e9-b210-d663bd873d93"
        , createdAt = Time.millisToPosix 1642607898
        }


editURLReferenceEvent : Event
editURLReferenceEvent =
    EditReferenceEvent
        (EditReferenceURLEvent
            { url =
                { changed = False
                , value = Nothing
                }
            , label =
                { changed = True
                , value = Just "Example"
                }
            , annotations =
                { changed = False
                , value = Nothing
                }
            }
        )
        { uuid = "349624f6-2dfc-11e9-b210-d663bd873d93"
        , entityUuid = "e559cf36-2dfc-11e9-b210-d663bd873d93"
        , parentUuid = "2f73c924-2dfc-11e9-b210-d663bd873d93"
        , createdAt = Time.millisToPosix 1642607898
        }


editCrossReferenceEvent : Event
editCrossReferenceEvent =
    EditReferenceEvent
        (EditReferenceCrossEvent
            { targetUuid =
                { changed = False
                , value = Nothing
                }
            , description =
                { changed = True
                , value = Just "See also"
                }
            , annotations =
                { changed = False
                , value = Nothing
                }
            }
        )
        { uuid = "349624f6-2dfc-11e9-b210-d663bd873d93"
        , entityUuid = "fe19113a-2dfc-11e9-b210-d663bd873d93"
        , parentUuid = "2f73c924-2dfc-11e9-b210-d663bd873d93"
        , createdAt = Time.millisToPosix 1642607898
        }


editReferenceEventTest : Test
editReferenceEventTest =
    describe "EditReferenceEvent"
        [ parametrized
            [ editResourcePageReferenceEvent, editURLReferenceEvent, editCrossReferenceEvent ]
            "should encode decode"
          <|
            \event ->
                expectEventEncodeDecode event
        , parametrized
            [ editResourcePageReferenceEvent, editURLReferenceEvent, editCrossReferenceEvent ]
            "get event uuid"
          <|
            \event ->
                Expect.equal "349624f6-2dfc-11e9-b210-d663bd873d93" (Event.getUuid event)
        , parametrized
            [ ( editResourcePageReferenceEvent, Just "ba931b74-6254-403e-a10e-ba14bd55e384" )
            , ( editURLReferenceEvent, Just "Example" )
            , ( editCrossReferenceEvent, Nothing )
            ]
            "get event entity visible name"
          <|
            \( event, name ) ->
                Expect.equal name (Event.getEntityVisibleName event)
        ]


deleteReferenceEvent : Event
deleteReferenceEvent =
    DeleteReferenceEvent
        { uuid = "349624f6-2dfc-11e9-b210-d663bd873d93"
        , entityUuid = "6606ebf8-2dff-11e9-b210-d663bd873d93"
        , parentUuid = "2f73c924-2dfc-11e9-b210-d663bd873d93"
        , createdAt = Time.millisToPosix 1642607898
        }


deleteReferenceEventTest : Test
deleteReferenceEventTest =
    describe "DeleteReferenceEvent"
        [ test "should encode and decode" <|
            \_ ->
                expectEventEncodeDecode deleteReferenceEvent
        , test "get event uuid" <|
            \_ ->
                Expect.equal "349624f6-2dfc-11e9-b210-d663bd873d93" (Event.getUuid deleteReferenceEvent)
        , test "get entity visible name" <|
            \_ ->
                Expect.equal Nothing (Event.getEntityVisibleName deleteReferenceEvent)
        ]


moveReferenceEvent : Event
moveReferenceEvent =
    MoveReferenceEvent
        { targetUuid = "71268f6c-04bd-4d83-9418-318c619e7444" }
        { uuid = "349624f6-2dfc-11e9-b210-d663bd873d93"
        , entityUuid = "bad22d1c-2e01-11e9-b210-d663bd873d93"
        , parentUuid = "2f73c924-2dfc-11e9-b210-d663bd873d93"
        , createdAt = Time.millisToPosix 1642607898
        }


moveReferenceEventTest : Test
moveReferenceEventTest =
    describe "MoveReferenceEvent"
        [ test "should encode and decode" <|
            \_ -> expectEventEncodeDecode moveReferenceEvent
        , test "get event uuid" <|
            \_ ->
                Expect.equal "349624f6-2dfc-11e9-b210-d663bd873d93" (Event.getUuid moveReferenceEvent)
        , test "get event entity visible name" <|
            \_ ->
                Expect.equal Nothing (Event.getEntityVisibleName moveReferenceEvent)
        ]



{- expert events -}


addExpertEvent : Event
addExpertEvent =
    AddExpertEvent
        { name = "Albert Einstein"
        , email = "albert.einstein@example.com"
        , annotations = []
        }
        { uuid = "349624f6-2dfc-11e9-b210-d663bd873d93"
        , entityUuid = "bad22d1c-2e01-11e9-b210-d663bd873d93"
        , parentUuid = "2f73c924-2dfc-11e9-b210-d663bd873d93"
        , createdAt = Time.millisToPosix 1642607898
        }


addExpertEventTest : Test
addExpertEventTest =
    describe "AddExpertEvent"
        [ test "should encode and decode" <|
            \_ -> expectEventEncodeDecode addExpertEvent
        , test "get event uuid" <|
            \_ ->
                Expect.equal "349624f6-2dfc-11e9-b210-d663bd873d93" (Event.getUuid addExpertEvent)
        , test "get event entity visible name" <|
            \_ ->
                Expect.equal (Just "Albert Einstein") (Event.getEntityVisibleName addExpertEvent)
        ]


editExpertEvent : Event
editExpertEvent =
    EditExpertEvent
        { name =
            { changed = True
            , value = Just "Nikola Tesla"
            }
        , email =
            { changed = False
            , value = Nothing
            }
        , annotations =
            { changed = False
            , value = Nothing
            }
        }
        { uuid = "349624f6-2dfc-11e9-b210-d663bd873d93"
        , entityUuid = "bad22d1c-2e01-11e9-b210-d663bd873d93"
        , parentUuid = "2f73c924-2dfc-11e9-b210-d663bd873d93"
        , createdAt = Time.millisToPosix 1642607898
        }


editExpertEventTest : Test
editExpertEventTest =
    describe "EditExpertEvent"
        [ test "should encode and decode" <|
            \_ -> expectEventEncodeDecode editExpertEvent
        , test "get event uuid" <|
            \_ ->
                Expect.equal "349624f6-2dfc-11e9-b210-d663bd873d93" (Event.getUuid editExpertEvent)
        , test "get event entity visible name when not changed" <|
            \_ ->
                let
                    event =
                        EditExpertEvent
                            { name =
                                { changed = False
                                , value = Nothing
                                }
                            , email =
                                { changed = True
                                , value = Just "nikola.tesla@example.com"
                                }
                            , annotations =
                                { changed = False
                                , value = Nothing
                                }
                            }
                            { uuid = "349624f6-2dfc-11e9-b210-d663bd873d93"
                            , entityUuid = "bad22d1c-2e01-11e9-b210-d663bd873d93"
                            , parentUuid = "2f73c924-2dfc-11e9-b210-d663bd873d93"
                            , createdAt = Time.millisToPosix 1642607898
                            }
                in
                Expect.equal Nothing (Event.getEntityVisibleName event)
        , test "get event entity visible name when changed" <|
            \_ ->
                Expect.equal (Just "Nikola Tesla") (Event.getEntityVisibleName editExpertEvent)
        ]


deleteExpertEvent : Event
deleteExpertEvent =
    DeleteExpertEvent
        { uuid = "349624f6-2dfc-11e9-b210-d663bd873d93"
        , entityUuid = "bad22d1c-2e01-11e9-b210-d663bd873d93"
        , parentUuid = "2f73c924-2dfc-11e9-b210-d663bd873d93"
        , createdAt = Time.millisToPosix 1642607898
        }


deleteExpertEventTest : Test
deleteExpertEventTest =
    describe "DeleteExpertEvent"
        [ test "should encode and decode" <|
            \_ ->
                expectEventEncodeDecode deleteExpertEvent
        , test "get event uuid" <|
            \_ ->
                Expect.equal "349624f6-2dfc-11e9-b210-d663bd873d93" (Event.getUuid deleteExpertEvent)
        , test "get entity visible name" <|
            \_ ->
                Expect.equal Nothing (Event.getEntityVisibleName deleteExpertEvent)
        ]


moveExpertEvent : Event
moveExpertEvent =
    MoveExpertEvent
        { targetUuid = "71268f6c-04bd-4d83-9418-318c619e7444" }
        { uuid = "349624f6-2dfc-11e9-b210-d663bd873d93"
        , entityUuid = "bad22d1c-2e01-11e9-b210-d663bd873d93"
        , parentUuid = "2f73c924-2dfc-11e9-b210-d663bd873d93"
        , createdAt = Time.millisToPosix 1642607898
        }


moveExpertEventTest : Test
moveExpertEventTest =
    describe "MoveExpertEvent"
        [ test "should encode and decode" <|
            \_ -> expectEventEncodeDecode moveExpertEvent
        , test "get event uuid" <|
            \_ ->
                Expect.equal "349624f6-2dfc-11e9-b210-d663bd873d93" (Event.getUuid moveExpertEvent)
        , test "get event entity visible name" <|
            \_ ->
                Expect.equal Nothing (Event.getEntityVisibleName moveExpertEvent)
        ]



{- resource collection events -}


addResourceCollectionEvent : Event
addResourceCollectionEvent =
    AddResourceCollectionEvent
        { title = "Collection"
        , annotations = []
        }
        { uuid = "349624f6-2dfc-11e9-b210-d663bd873d93"
        , entityUuid = "bad22d1c-2e01-11e9-b210-d663bd873d93"
        , parentUuid = "2f73c924-2dfc-11e9-b210-d663bd873d93"
        , createdAt = Time.millisToPosix 1642607898
        }


addResourceCollectionEventTest : Test
addResourceCollectionEventTest =
    describe "AddResourceCollectionEvent"
        [ test "should encode and decode" <|
            \_ -> expectEventEncodeDecode addResourceCollectionEvent
        , test "get event uuid" <|
            \_ ->
                Expect.equal "349624f6-2dfc-11e9-b210-d663bd873d93" (Event.getUuid addResourceCollectionEvent)
        , test "get event entity visible name" <|
            \_ ->
                Expect.equal (Just "Collection") (Event.getEntityVisibleName addResourceCollectionEvent)
        ]


editResourceCollectionEvent : Event
editResourceCollectionEvent =
    EditResourceCollectionEvent
        { title =
            { changed = True
            , value = Just "New Collection"
            }
        , resourcePageUuids =
            { changed = True
            , value = Just [ "ba931b74-6254-403e-a10e-ba14bd55e384", "bf2e14ca-67b9-4c00-b02e-b75213952992" ]
            }
        , annotations =
            { changed = False
            , value = Nothing
            }
        }
        { uuid = "349624f6-2dfc-11e9-b210-d663bd873d93"
        , entityUuid = "bad22d1c-2e01-11e9-b210-d663bd873d93"
        , parentUuid = "2f73c924-2dfc-11e9-b210-d663bd873d93"
        , createdAt = Time.millisToPosix 1642607898
        }


editResourceCollectionEventTest : Test
editResourceCollectionEventTest =
    describe "EditResourceCollectionEvent"
        [ test "should encode and decode" <|
            \_ -> expectEventEncodeDecode editResourceCollectionEvent
        , test "get event uuid" <|
            \_ ->
                Expect.equal "349624f6-2dfc-11e9-b210-d663bd873d93" (Event.getUuid editResourceCollectionEvent)
        , test "get event entity visible name when not changed" <|
            \_ ->
                let
                    event =
                        EditResourceCollectionEvent
                            { title =
                                { changed = False
                                , value = Nothing
                                }
                            , resourcePageUuids =
                                { changed = True
                                , value = Just [ "ba931b74-6254-403e-a10e-ba14bd55e384", "bf2e14ca-67b9-4c00-b02e-b75213952992" ]
                                }
                            , annotations =
                                { changed = False
                                , value = Nothing
                                }
                            }
                            { uuid = "349624f6-2dfc-11e9-b210-d663bd873d93"
                            , entityUuid = "bad22d1c-2e01-11e9-b210-d663bd873d93"
                            , parentUuid = "2f73c924-2dfc-11e9-b210-d663bd873d93"
                            , createdAt = Time.millisToPosix 1642607898
                            }
                in
                Expect.equal Nothing (Event.getEntityVisibleName event)
        , test "get event entity visible name when changed" <|
            \_ ->
                Expect.equal (Just "New Collection") (Event.getEntityVisibleName editResourceCollectionEvent)
        ]


deleteResourceCollectionEvent : Event
deleteResourceCollectionEvent =
    DeleteResourceCollectionEvent
        { uuid = "349624f6-2dfc-11e9-b210-d663bd873d93"
        , entityUuid = "bad22d1c-2e01-11e9-b210-d663bd873d93"
        , parentUuid = "2f73c924-2dfc-11e9-b210-d663bd873d93"
        , createdAt = Time.millisToPosix 1642607898
        }


deleteResourceCollectionEventTest : Test
deleteResourceCollectionEventTest =
    describe "DeleteResourceCollectionEvent"
        [ test "should encode and decode" <|
            \_ ->
                expectEventEncodeDecode deleteResourceCollectionEvent
        , test "get event uuid" <|
            \_ ->
                Expect.equal "349624f6-2dfc-11e9-b210-d663bd873d93" (Event.getUuid deleteResourceCollectionEvent)
        , test "get entity visible name" <|
            \_ ->
                Expect.equal Nothing (Event.getEntityVisibleName deleteResourceCollectionEvent)
        ]



{- resource page events -}


addResourcePageEvent : Event
addResourcePageEvent =
    AddResourcePageEvent
        { title = "Page"
        , content = "Content"
        , annotations = []
        }
        { uuid = "349624f6-2dfc-11e9-b210-d663bd873d93"
        , entityUuid = "bad22d1c-2e01-11e9-b210-d663bd873d93"
        , parentUuid = "2f73c924-2dfc-11e9-b210-d663bd873d93"
        , createdAt = Time.millisToPosix 1642607898
        }


addResourcePageEventTest : Test
addResourcePageEventTest =
    describe "AddResourcePageEvent"
        [ test "should encode and decode" <|
            \_ -> expectEventEncodeDecode addResourcePageEvent
        , test "get event uuid" <|
            \_ ->
                Expect.equal "349624f6-2dfc-11e9-b210-d663bd873d93" (Event.getUuid addResourcePageEvent)
        , test "get event entity visible name" <|
            \_ ->
                Expect.equal (Just "Page") (Event.getEntityVisibleName addResourcePageEvent)
        ]


editResourcePageEvent : Event
editResourcePageEvent =
    EditResourcePageEvent
        { title =
            { changed = True
            , value = Just "New Page"
            }
        , content =
            { changed = True
            , value = Just "New Content"
            }
        , annotations =
            { changed = False
            , value = Nothing
            }
        }
        { uuid = "349624f6-2dfc-11e9-b210-d663bd873d93"
        , entityUuid = "bad22d1c-2e01-11e9-b210-d663bd873d93"
        , parentUuid = "2f73c924-2dfc-11e9-b210-d663bd873d93"
        , createdAt = Time.millisToPosix 1642607898
        }


editResourcePageEventTest : Test
editResourcePageEventTest =
    describe "EditResourcePageEvent"
        [ test "should encode and decode" <|
            \_ -> expectEventEncodeDecode editResourcePageEvent
        , test "get event uuid" <|
            \_ ->
                Expect.equal "349624f6-2dfc-11e9-b210-d663bd873d93" (Event.getUuid editResourcePageEvent)
        , test "get event entity visible name when not changed" <|
            \_ ->
                let
                    event =
                        EditResourcePageEvent
                            { title =
                                { changed = False
                                , value = Nothing
                                }
                            , content =
                                { changed = True
                                , value = Just "New Content"
                                }
                            , annotations =
                                { changed = False
                                , value = Nothing
                                }
                            }
                            { uuid = "349624f6-2dfc-11e9-b210-d663bd873d93"
                            , entityUuid = "bad22d1c-2e01-11e9-b210-d663bd873d93"
                            , parentUuid = "2f73c924-2dfc-11e9-b210-d663bd873d93"
                            , createdAt = Time.millisToPosix 1642607898
                            }
                in
                Expect.equal Nothing (Event.getEntityVisibleName event)
        , test "get event entity visible name when changed" <|
            \_ ->
                Expect.equal (Just "New Page") (Event.getEntityVisibleName editResourcePageEvent)
        ]


deleteResourcePageEvent : Event
deleteResourcePageEvent =
    DeleteResourcePageEvent
        { uuid = "349624f6-2dfc-11e9-b210-d663bd873d93"
        , entityUuid = "bad22d1c-2e01-11e9-b210-d663bd873d93"
        , parentUuid = "2f73c924-2dfc-11e9-b210-d663bd873d93"
        , createdAt = Time.millisToPosix 1642607898
        }


deleteResourcePageEventTest : Test
deleteResourcePageEventTest =
    describe "DeleteResourcePageEvent"
        [ test "should encode and decode" <|
            \_ ->
                expectEventEncodeDecode deleteResourcePageEvent
        , test "get event uuid" <|
            \_ ->
                Expect.equal "349624f6-2dfc-11e9-b210-d663bd873d93" (Event.getUuid deleteResourcePageEvent)
        , test "get entity visible name" <|
            \_ ->
                Expect.equal Nothing (Event.getEntityVisibleName deleteResourcePageEvent)
        ]



{- test utils -}


expectEventEncodeDecode : Event -> Expectation
expectEventEncodeDecode =
    expectEncodeDecode Event.encode Event.decoder
