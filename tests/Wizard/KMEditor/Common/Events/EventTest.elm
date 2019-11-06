module Wizard.KMEditor.Common.Events.EventTest exposing
    ( addAnswerEventTest
    , addChapterEventTest
    , addExpertEventTest
    , addIntegrationEventTest
    , addQuestionEventTest
    , addReferenceEventTest
    , addTagEventTest
    , deleteAnswerEventTest
    , deleteChapterEventTest
    , deleteExpertEventTest
    , deleteIntegrationEventTest
    , deleteQuestionEventTest
    , deleteReferenceEventTest
    , deleteTagEventTest
    , editAnswerEventTest
    , editChapterEventTest
    , editExpertEventTest
    , editIntegrationEventTest
    , editKnowledgeModelEventTest
    , editQuestionEventTest
    , editReferenceEventTest
    , editTagEventTest
    )

import Dict
import Expect exposing (Expectation)
import Test exposing (..)
import TestUtils exposing (expectEncodeDecode, parametrized)
import Wizard.KMEditor.Common.Events.AddQuestionEventData exposing (AddQuestionEventData(..))
import Wizard.KMEditor.Common.Events.AddReferenceEventData exposing (AddReferenceEventData(..))
import Wizard.KMEditor.Common.Events.EditQuestionEventData exposing (EditQuestionEventData(..))
import Wizard.KMEditor.Common.Events.EditReferenceEventData exposing (EditReferenceEventData(..))
import Wizard.KMEditor.Common.Events.Event as Event exposing (Event(..))
import Wizard.KMEditor.Common.KnowledgeModel.Question.QuestionValueType exposing (QuestionValueType(..))
import Wizard.Utils exposing (nilUuid)



{- knowledge model events -}


editKnowledgeModelEvent : Event
editKnowledgeModelEvent =
    EditKnowledgeModelEvent
        { name =
            { changed = True
            , value = Just "My Knowledge Model"
            }
        , chapterUuids =
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
        }
        { uuid = "79d1e7b4-c2d8-49ff-8293-dfcfdb6da6ac"
        , entityUuid = "aad436a7-c8a5-4237-a2bd-34decdf26a1f"
        , parentUuid = nilUuid
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
                let
                    event =
                        EditKnowledgeModelEvent
                            { name =
                                { changed = False
                                , value = Nothing
                                }
                            , chapterUuids =
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
                            }
                            { uuid = "79d1e7b4-c2d8-49ff-8293-dfcfdb6da6ac"
                            , entityUuid = "aad436a7-c8a5-4237-a2bd-34decdf26a1f"
                            , parentUuid = nilUuid
                            }
                in
                Expect.equal Nothing (Event.getEntityVisibleName event)
        , test "get event entity visible name when changed" <|
            \_ ->
                Expect.equal (Just "My Knowledge Model") (Event.getEntityVisibleName editKnowledgeModelEvent)
        ]



{- chapter events -}


addChapterEvent : Event
addChapterEvent =
    AddChapterEvent
        { title = "Design of Experiment"
        , text = Just "This is a chapter about the designing of the experiment"
        }
        { uuid = "485bc170-2df3-11e9-b210-d663bd873d93"
        , entityUuid = "42d0bd1e-2df3-11e9-b210-d663bd873d93"
        , parentUuid = "aad436a7-c8a5-4237-a2bd-34decdf26a1f"
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
        }
        { uuid = "485bc170-2df3-11e9-b210-d663bd873d93"
        , entityUuid = "42d0bd1e-2df3-11e9-b210-d663bd873d93"
        , parentUuid = "aad436a7-c8a5-4237-a2bd-34decdf26a1f"
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
                            }
                            { uuid = "485bc170-2df3-11e9-b210-d663bd873d93"
                            , entityUuid = "42d0bd1e-2df3-11e9-b210-d663bd873d93"
                            , parentUuid = "aad436a7-c8a5-4237-a2bd-34decdf26a1f"
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



{- tag events -}


addTagEvent : Event
addTagEvent =
    AddTagEvent
        { name = "Astronomy"
        , description = Just "Questions connected to astronomy"
        , color = "#F5A623"
        }
        { uuid = "485bc170-2df3-11e9-b210-d663bd873d93"
        , entityUuid = "1cf9c1f2-2df9-11e9-b210-d663bd873d93"
        , parentUuid = "aad436a7-c8a5-4237-a2bd-34decdf26a1f"
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
        }
        { uuid = "485bc170-2df3-11e9-b210-d663bd873d93"
        , entityUuid = "1cf9c1f2-2df9-11e9-b210-d663bd873d93"
        , parentUuid = "aad436a7-c8a5-4237-a2bd-34decdf26a1f"
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
                            }
                            { uuid = "485bc170-2df3-11e9-b210-d663bd873d93"
                            , entityUuid = "1cf9c1f2-2df9-11e9-b210-d663bd873d93"
                            , parentUuid = "aad436a7-c8a5-4237-a2bd-34decdf26a1f"
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


addIntegrationEvent : Event
addIntegrationEvent =
    AddIntegrationEvent
        { id = "service"
        , name = "Service"
        , props = [ "kind", "category" ]
        , logo = "data:image/png;base64,..."
        , requestMethod = "GET"
        , requestUrl = "/api/search"
        , requestHeaders = Dict.fromList [ ( "X_SEARCH", "full" ), ( "X_USER", "user" ) ]
        , requestBody = "{}"
        , responseListField = "items"
        , responseIdField = "uuid"
        , responseNameField = "title"
        , itemUrl = "http://example.com/${id}"
        }
        { uuid = "cbecbad5-f85d-4e7e-95b9-34669e3333f9"
        , entityUuid = "0d03f237-bc95-4033-99ab-5ba3d85cd6c7"
        , parentUuid = "aad436a7-c8a5-4237-a2bd-34decdf26a1f"
        }


addIntegrationEventTest : Test
addIntegrationEventTest =
    describe "AddIntegrationEvent"
        [ test "should encode and decode" <|
            \_ -> expectEventEncodeDecode addIntegrationEvent
        , test "get event uuid" <|
            \_ ->
                Expect.equal "cbecbad5-f85d-4e7e-95b9-34669e3333f9" (Event.getUuid addIntegrationEvent)
        , test "get event entity visible name" <|
            \_ ->
                Expect.equal (Just "Service") (Event.getEntityVisibleName addIntegrationEvent)
        ]


editIntegrationEvent : Event
editIntegrationEvent =
    EditIntegrationEvent
        { id =
            { changed = True
            , value = Just "service"
            }
        , name =
            { changed = True
            , value = Just "Service"
            }
        , props =
            { changed = True
            , value = Just [ "kind", "category" ]
            }
        , logo =
            { changed = False
            , value = Nothing
            }
        , requestMethod =
            { changed = True
            , value = Just "GET"
            }
        , requestUrl =
            { changed = False
            , value = Nothing
            }
        , requestHeaders =
            { changed = True
            , value = Just <| Dict.fromList [ ( "X_SEARCH", "full" ), ( "X_USER", "user" ) ]
            }
        , requestBody =
            { changed = True
            , value = Just "{}"
            }
        , responseListField =
            { changed = False
            , value = Nothing
            }
        , responseIdField =
            { changed = False
            , value = Nothing
            }
        , responseNameField =
            { changed = True
            , value = Just "title"
            }
        , itemUrl =
            { changed = True
            , value = Just "http://example.com/${id}"
            }
        }
        { uuid = "cbecbad5-f85d-4e7e-95b9-34669e3333f9"
        , entityUuid = "52034933-3065-4876-9999-5f5c0d91f7aa"
        , parentUuid = "aad436a7-c8a5-4237-a2bd-34decdf26a1f"
        }


editIntegrationEventTest : Test
editIntegrationEventTest =
    describe "EditIntegrationEventTest"
        [ test "should encode and decode" <|
            \_ -> expectEventEncodeDecode editIntegrationEvent
        , test "get event uuid" <|
            \_ ->
                Expect.equal "cbecbad5-f85d-4e7e-95b9-34669e3333f9" (Event.getUuid editIntegrationEvent)
        , test "get event entity visible name" <|
            \_ ->
                Expect.equal (Just "Service") (Event.getEntityVisibleName editIntegrationEvent)
        ]


deleteIntegrationEvent : Event
deleteIntegrationEvent =
    DeleteIntegrationEvent
        { uuid = "cbecbad5-f85d-4e7e-95b9-34669e3333f9"
        , entityUuid = "52034933-3065-4876-9999-5f5c0d91f7aa"
        , parentUuid = "aad436a7-c8a5-4237-a2bd-34decdf26a1f"
        }


deleteIntegrationEventTest : Test
deleteIntegrationEventTest =
    describe "DeleteIntegrationEvent"
        [ test "should encode and decode" <|
            \_ -> expectEventEncodeDecode editIntegrationEvent
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
            , requiredLevel = Just 2
            , tagUuids = []
            }
        )
        { uuid = "b09ed98c-3043-11e9-b210-d663bd873d93"
        , entityUuid = "a5405e3a-3043-11e9-b210-d663bd873d93"
        , parentUuid = "42d0bd1e-2df3-11e9-b210-d663bd873d93"
        }


addListQuestionEvent : Event
addListQuestionEvent =
    AddQuestionEvent
        (AddQuestionListEvent
            { title = "Can you answer this question?"
            , text = Just "Just answer the question!"
            , requiredLevel = Just 2
            , tagUuids = []
            }
        )
        { uuid = "b09ed98c-3043-11e9-b210-d663bd873d93"
        , entityUuid = "a5405e3a-3043-11e9-b210-d663bd873d93"
        , parentUuid = "42d0bd1e-2df3-11e9-b210-d663bd873d93"
        }


addValueQuestionEvent : Event
addValueQuestionEvent =
    AddQuestionEvent
        (AddQuestionValueEvent
            { title = "Can you answer this question?"
            , text = Nothing
            , requiredLevel = Nothing
            , tagUuids = [ "dc1dcc8a-3043-11e9-b210-d663bd873d93", "dc1dcf00-3043-11e9-b210-d663bd873d93" ]
            , valueType = NumberQuestionValueType
            }
        )
        { uuid = "b09ed98c-3043-11e9-b210-d663bd873d93"
        , entityUuid = "a5405e3a-3043-11e9-b210-d663bd873d93"
        , parentUuid = "42d0bd1e-2df3-11e9-b210-d663bd873d93"
        }


addIntegrationQuestionEvent : Event
addIntegrationQuestionEvent =
    AddQuestionEvent
        (AddQuestionIntegrationEvent
            { title = "Can you answer this question?"
            , text = Nothing
            , requiredLevel = Nothing
            , tagUuids = [ "dc1dcc8a-3043-11e9-b210-d663bd873d93", "dc1dcf00-3043-11e9-b210-d663bd873d93" ]
            , integrationUuid = "1d522339-e93b-44e9-bc2a-1df65fb97dc6"
            , props = Dict.fromList [ ( "prop1", "value1" ), ( "prop2", "value2" ) ]
            }
        )
        { uuid = "b09ed98c-3043-11e9-b210-d663bd873d93"
        , entityUuid = "a5405e3a-3043-11e9-b210-d663bd873d93"
        , parentUuid = "42d0bd1e-2df3-11e9-b210-d663bd873d93"
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
            , requiredLevel = { changed = True, value = Just (Just 2) }
            , tagUuids = { changed = False, value = Nothing }
            , referenceUuids = { changed = False, value = Nothing }
            , expertUuids = { changed = True, value = Just [ "fe1b440e-3046-11e9-b210-d663bd873d93" ] }
            , answerUuids = { changed = True, value = Just [ "5cb0bedc-3046-11e9-b210-d663bd873d93", "5cb0c15c-3046-11e9-b210-d663bd873d93" ] }
            }
        )
        { uuid = "b09ed98c-3043-11e9-b210-d663bd873d93"
        , entityUuid = "a5405e3a-3043-11e9-b210-d663bd873d93"
        , parentUuid = "42d0bd1e-2df3-11e9-b210-d663bd873d93"
        }


editListQuestionEvent : Event
editListQuestionEvent =
    EditQuestionEvent
        (EditQuestionListEvent
            { title = { changed = True, value = Just "This is a new title" }
            , text = { changed = False, value = Nothing }
            , requiredLevel = { changed = False, value = Nothing }
            , tagUuids = { changed = False, value = Nothing }
            , referenceUuids = { changed = True, value = Just [ "f749367c-3046-11e9-b210-d663bd873d93" ] }
            , expertUuids = { changed = False, value = Nothing }
            , itemTemplateQuestionUuids = { changed = True, value = Just [ "b2c867fc-3046-11e9-b210-d663bd873d93" ] }
            }
        )
        { uuid = "b09ed98c-3043-11e9-b210-d663bd873d93"
        , entityUuid = "a5405e3a-3043-11e9-b210-d663bd873d93"
        , parentUuid = "42d0bd1e-2df3-11e9-b210-d663bd873d93"
        }


editValueQuestionEvent : Event
editValueQuestionEvent =
    EditQuestionEvent
        (EditQuestionValueEvent
            { title = { changed = True, value = Just "What date is today?" }
            , text = { changed = False, value = Nothing }
            , requiredLevel = { changed = True, value = Just (Just 2) }
            , tagUuids = { changed = True, value = Just [ "e734907e-3046-11e9-b210-d663bd873d93", "e73495ce-3046-11e9-b210-d663bd873d93", "e7349740-3046-11e9-b210-d663bd873d93" ] }
            , referenceUuids = { changed = False, value = Nothing }
            , expertUuids = { changed = False, value = Nothing }
            , valueType = { changed = True, value = Just DateQuestionValueType }
            }
        )
        { uuid = "b09ed98c-3043-11e9-b210-d663bd873d93"
        , entityUuid = "a5405e3a-3043-11e9-b210-d663bd873d93"
        , parentUuid = "42d0bd1e-2df3-11e9-b210-d663bd873d93"
        }


editIntegrationQuestionEvent : Event
editIntegrationQuestionEvent =
    EditQuestionEvent
        (EditQuestionIntegrationEvent
            { title = { changed = True, value = Just "What database will you use?" }
            , text = { changed = False, value = Nothing }
            , requiredLevel = { changed = True, value = Just (Just 2) }
            , tagUuids = { changed = True, value = Just [ "e734907e-3046-11e9-b210-d663bd873d93", "e73495ce-3046-11e9-b210-d663bd873d93", "e7349740-3046-11e9-b210-d663bd873d93" ] }
            , referenceUuids = { changed = False, value = Nothing }
            , expertUuids = { changed = False, value = Nothing }
            , integrationUuid = { changed = True, value = Just "1d522339-e93b-44e9-bc2a-1df65fb97dc6" }
            , props = { changed = True, value = Just <| Dict.fromList [ ( "prop1", "value1" ), ( "prop2", "value2" ) ] }
            }
        )
        { uuid = "b09ed98c-3043-11e9-b210-d663bd873d93"
        , entityUuid = "a5405e3a-3043-11e9-b210-d663bd873d93"
        , parentUuid = "42d0bd1e-2df3-11e9-b210-d663bd873d93"
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
        }
        { uuid = "efac9f6e-2e00-11e9-b210-d663bd873d93"
        , entityUuid = "2bbe5372-2e00-11e9-b210-d663bd873d93"
        , parentUuid = "2f73c924-2dfc-11e9-b210-d663bd873d93"
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
        }
        { uuid = "efac9f6e-2e00-11e9-b210-d663bd873d93"
        , entityUuid = "2bbe5372-2e00-11e9-b210-d663bd873d93"
        , parentUuid = "2f73c924-2dfc-11e9-b210-d663bd873d93"
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
                            }
                            { uuid = "efac9f6e-2e00-11e9-b210-d663bd873d93"
                            , entityUuid = "2bbe5372-2e00-11e9-b210-d663bd873d93"
                            , parentUuid = "2f73c924-2dfc-11e9-b210-d663bd873d93"
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



{- reference events -}


addResourcePageReferenceEvent : Event
addResourcePageReferenceEvent =
    AddReferenceEvent
        (AddReferenceResourcePageEvent
            { shortUuid = "atq"
            }
        )
        { uuid = "349624f6-2dfc-11e9-b210-d663bd873d93"
        , entityUuid = "3f52e8fc-2dfc-11e9-b210-d663bd873d93"
        , parentUuid = "2f73c924-2dfc-11e9-b210-d663bd873d93"
        }


addURLReferenceEvent : Event
addURLReferenceEvent =
    AddReferenceEvent
        (AddReferenceURLEvent
            { url = "http://example.com"
            , label = "Example"
            }
        )
        { uuid = "349624f6-2dfc-11e9-b210-d663bd873d93"
        , entityUuid = "e559cf36-2dfc-11e9-b210-d663bd873d93"
        , parentUuid = "2f73c924-2dfc-11e9-b210-d663bd873d93"
        }


addCrossReferenceEvent : Event
addCrossReferenceEvent =
    AddReferenceEvent
        (AddReferenceCrossEvent
            { targetUuid = "072af95a-2dfd-11e9-b210-d663bd873d93"
            , description = "Related"
            }
        )
        { uuid = "349624f6-2dfc-11e9-b210-d663bd873d93"
        , entityUuid = "fe19113a-2dfc-11e9-b210-d663bd873d93"
        , parentUuid = "2f73c924-2dfc-11e9-b210-d663bd873d93"
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
            [ ( addResourcePageReferenceEvent, "atq" )
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
            { shortUuid =
                { changed = True
                , value = Just "atq"
                }
            }
        )
        { uuid = "349624f6-2dfc-11e9-b210-d663bd873d93"
        , entityUuid = "3f52e8fc-2dfc-11e9-b210-d663bd873d93"
        , parentUuid = "2f73c924-2dfc-11e9-b210-d663bd873d93"
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
            }
        )
        { uuid = "349624f6-2dfc-11e9-b210-d663bd873d93"
        , entityUuid = "e559cf36-2dfc-11e9-b210-d663bd873d93"
        , parentUuid = "2f73c924-2dfc-11e9-b210-d663bd873d93"
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
            }
        )
        { uuid = "349624f6-2dfc-11e9-b210-d663bd873d93"
        , entityUuid = "fe19113a-2dfc-11e9-b210-d663bd873d93"
        , parentUuid = "2f73c924-2dfc-11e9-b210-d663bd873d93"
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
            [ ( editResourcePageReferenceEvent, Just "atq" )
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



{- expert events -}


addExpertEvent : Event
addExpertEvent =
    AddExpertEvent
        { name = "Albert Einstein"
        , email = "albert.einstein@example.com"
        }
        { uuid = "349624f6-2dfc-11e9-b210-d663bd873d93"
        , entityUuid = "bad22d1c-2e01-11e9-b210-d663bd873d93"
        , parentUuid = "2f73c924-2dfc-11e9-b210-d663bd873d93"
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
        }
        { uuid = "349624f6-2dfc-11e9-b210-d663bd873d93"
        , entityUuid = "bad22d1c-2e01-11e9-b210-d663bd873d93"
        , parentUuid = "2f73c924-2dfc-11e9-b210-d663bd873d93"
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
                            }
                            { uuid = "349624f6-2dfc-11e9-b210-d663bd873d93"
                            , entityUuid = "bad22d1c-2e01-11e9-b210-d663bd873d93"
                            , parentUuid = "2f73c924-2dfc-11e9-b210-d663bd873d93"
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



{- test utils -}


expectEventEncodeDecode : Event -> Expectation
expectEventEncodeDecode =
    expectEncodeDecode Event.encode Event.decoder
