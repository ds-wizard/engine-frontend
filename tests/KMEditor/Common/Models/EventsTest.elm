module KMEditor.Common.Models.EventsTest exposing
    ( addAnswerEventTest
    , addChapterEventTest
    , addExpertEventTest
    , addReferenceEventTest
    , addTagEventTest
    , deleteAnswerEventTest
    , deleteChapterEventTest
    , deleteExpertEventTest
    , deleteReferenceEventTest
    , deleteTagEventTest
    , editAnswerEventTest
    , editChapterEventTest
    , editExpertEventTest
    , editKnowledgeModelEventTest
    , editReferenceEventTest
    , editTagEventTest
    , eventFieldTest
    )

import Expect exposing (Expectation)
import Json.Decode as Decode
import Json.Encode as Encode
import KMEditor.Common.Models.Events exposing (..)
import KMEditor.Common.Models.Path exposing (PathNode(..))
import Test exposing (..)
import TestUtils exposing (expectEncodeDecode, parametrized)



{- eventField -}


eventFieldTest : Test
eventFieldTest =
    describe "EventField"
        [ test "should encode and decode when not changed" <|
            \_ ->
                let
                    eventField =
                        { changed = False
                        , value = Nothing
                        }
                in
                expectEncodeDecode (encodeEventField Encode.string) (eventFieldDecoder Decode.string) eventField
        , test "should encode and decode when changed" <|
            \_ ->
                let
                    eventField =
                        { changed = True
                        , value = Just "My new value"
                        }
                in
                expectEncodeDecode (encodeEventField Encode.string) (eventFieldDecoder Decode.string) eventField
        , test "get value when not changed" <|
            \_ ->
                let
                    eventField =
                        { changed = False
                        , value = Nothing
                        }
                in
                Expect.equal Nothing (getEventFieldValue eventField)
        , test "get value when changed" <|
            \_ ->
                let
                    eventField =
                        { changed = True
                        , value = Just 12
                        }
                in
                Expect.equal (Just 12) (getEventFieldValue eventField)
        , test "get value with default when not changed" <|
            \_ ->
                let
                    eventField =
                        { changed = False
                        , value = Nothing
                        }
                in
                Expect.equal "Default" (getEventFieldValueWithDefault eventField "Default")
        , test "get value with default when changed" <|
            \_ ->
                let
                    eventField =
                        { changed = True
                        , value = Just [ 1, 2, 3 ]
                        }
                in
                Expect.equal [ 1, 2, 3 ] (getEventFieldValueWithDefault eventField [])
        ]



{- knowledge model events -}


editKnowledgeModelEvent : Event
editKnowledgeModelEvent =
    EditKnowledgeModelEvent
        { kmUuid = "aad436a7-c8a5-4237-a2bd-34decdf26a1f"
        , name =
            { changed = True
            , value = Just "My Knowledge Model"
            }
        , chapterIds =
            { changed = False
            , value = Nothing
            }
        , tagIds =
            { changed = False
            , value = Nothing
            }
        }
        { uuid = "79d1e7b4-c2d8-49ff-8293-dfcfdb6da6ac"
        , path = []
        }


editKnowledgeModelEventTest : Test
editKnowledgeModelEventTest =
    describe "EditKnowledgeModel"
        [ test "should encode and decode" <|
            \_ ->
                expectEventEncodeDecode editKnowledgeModelEvent
        , test "get event uuid" <|
            \_ ->
                Expect.equal "79d1e7b4-c2d8-49ff-8293-dfcfdb6da6ac" (getEventUuid editKnowledgeModelEvent)
        , test "get event entity visible name when not changed" <|
            \_ ->
                let
                    event =
                        EditKnowledgeModelEvent
                            { kmUuid = "aad436a7-c8a5-4237-a2bd-34decdf26a1f"
                            , name =
                                { changed = False
                                , value = Nothing
                                }
                            , chapterIds =
                                { changed = False
                                , value = Nothing
                                }
                            , tagIds =
                                { changed = False
                                , value = Nothing
                                }
                            }
                            { uuid = "79d1e7b4-c2d8-49ff-8293-dfcfdb6da6ac"
                            , path = []
                            }
                in
                Expect.equal Nothing (getEventEntityVisibleName event)
        , test "get event entity visible name when changed" <|
            \_ ->
                Expect.equal (Just "My Knowledge Model") (getEventEntityVisibleName editKnowledgeModelEvent)
        ]



{- chapter events -}


addChapterEvent : Event
addChapterEvent =
    AddChapterEvent
        { chapterUuid = "42d0bd1e-2df3-11e9-b210-d663bd873d93"
        , title = "Design of Experiment"
        , text = "This is a chapter about the designing of the experiment"
        }
        { uuid = "485bc170-2df3-11e9-b210-d663bd873d93"
        , path = [ KMPathNode "aad436a7-c8a5-4237-a2bd-34decdf26a1f" ]
        }


addChapterEventTest : Test
addChapterEventTest =
    describe "AddChapterEvent"
        [ test "should encode and decode" <|
            \_ ->
                expectEventEncodeDecode addChapterEvent
        , test "get event uuid" <|
            \_ ->
                Expect.equal "485bc170-2df3-11e9-b210-d663bd873d93" (getEventUuid addChapterEvent)
        , test "get event entity visible name" <|
            \_ ->
                Expect.equal (Just "Design of Experiment") (getEventEntityVisibleName addChapterEvent)
        ]


editChapterEvent : Event
editChapterEvent =
    EditChapterEvent
        { chapterUuid = "42d0bd1e-2df3-11e9-b210-d663bd873d93"
        , title =
            { changed = True
            , value = Just "Design of Experiment"
            }
        , text =
            { changed = False
            , value = Nothing
            }
        , questionIds =
            { changed = True
            , value = Just [ "2877dc7e-2df6-11e9-b210-d663bd873d93", "2877df94-2df6-11e9-b210-d663bd873d93" ]
            }
        }
        { uuid = "485bc170-2df3-11e9-b210-d663bd873d93"
        , path = [ KMPathNode "aad436a7-c8a5-4237-a2bd-34decdf26a1f" ]
        }


editChapterEventTest : Test
editChapterEventTest =
    describe "EditChapterEvent"
        [ test "should decode and encode" <|
            \_ ->
                expectEventEncodeDecode editChapterEvent
        , test "get event uuid" <|
            \_ ->
                Expect.equal "485bc170-2df3-11e9-b210-d663bd873d93" (getEventUuid editChapterEvent)
        , test "get entity visible name when not changed" <|
            \_ ->
                let
                    event =
                        EditChapterEvent
                            { chapterUuid = "42d0bd1e-2df3-11e9-b210-d663bd873d93"
                            , title =
                                { changed = False
                                , value = Nothing
                                }
                            , text =
                                { changed = False
                                , value = Nothing
                                }
                            , questionIds =
                                { changed = True
                                , value = Just [ "2877dc7e-2df6-11e9-b210-d663bd873d93", "2877df94-2df6-11e9-b210-d663bd873d93" ]
                                }
                            }
                            { uuid = "485bc170-2df3-11e9-b210-d663bd873d93"
                            , path = [ KMPathNode "aad436a7-c8a5-4237-a2bd-34decdf26a1f" ]
                            }
                in
                Expect.equal Nothing (getEventEntityVisibleName event)
        , test "get entity visible name when changed" <|
            \_ ->
                Expect.equal (Just "Design of Experiment") (getEventEntityVisibleName editChapterEvent)
        ]


deleteChapterEvent : Event
deleteChapterEvent =
    DeleteChapterEvent
        { chapterUuid = "42d0bd1e-2df3-11e9-b210-d663bd873d93"
        }
        { uuid = "485bc170-2df3-11e9-b210-d663bd873d93"
        , path = [ KMPathNode "aad436a7-c8a5-4237-a2bd-34decdf26a1f" ]
        }


deleteChapterEventTest : Test
deleteChapterEventTest =
    describe "DeleteChapterEvent"
        [ test "should encode and decode" <|
            \_ ->
                expectEventEncodeDecode deleteChapterEvent
        , test "get event uuid" <|
            \_ ->
                Expect.equal "485bc170-2df3-11e9-b210-d663bd873d93" (getEventUuid deleteChapterEvent)
        , test "get entity visible name" <|
            \_ ->
                Expect.equal Nothing (getEventEntityVisibleName deleteChapterEvent)
        ]



{- tag events -}


addTagEvent : Event
addTagEvent =
    AddTagEvent
        { tagUuid = "1cf9c1f2-2df9-11e9-b210-d663bd873d93"
        , name = "Astronomy"
        , description = Just "Questions connected to astronomy"
        , color = "#F5A623"
        }
        { uuid = "485bc170-2df3-11e9-b210-d663bd873d93"
        , path = [ KMPathNode "aad436a7-c8a5-4237-a2bd-34decdf26a1f" ]
        }


addTagEventTest : Test
addTagEventTest =
    describe "AddTagEvent"
        [ test "should encode and decode" <|
            \_ -> expectEventEncodeDecode addTagEvent
        , test "get event uuid" <|
            \_ ->
                Expect.equal "485bc170-2df3-11e9-b210-d663bd873d93" (getEventUuid addTagEvent)
        , test "get event entity visible name" <|
            \_ ->
                Expect.equal (Just "Astronomy") (getEventEntityVisibleName addTagEvent)
        ]


editTagEvent : Event
editTagEvent =
    EditTagEvent
        { tagUuid = "1cf9c1f2-2df9-11e9-b210-d663bd873d93"
        , name =
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
        , path = [ KMPathNode "aad436a7-c8a5-4237-a2bd-34decdf26a1f" ]
        }


editTagEventTest : Test
editTagEventTest =
    describe "EditTagEvent"
        [ test "should encode and decode" <|
            \_ -> expectEventEncodeDecode editTagEvent
        , test "get event uuid" <|
            \_ ->
                Expect.equal "485bc170-2df3-11e9-b210-d663bd873d93" (getEventUuid editTagEvent)
        , test "get event entity visible name when not changed" <|
            \_ ->
                let
                    event =
                        EditTagEvent
                            { tagUuid = "1cf9c1f2-2df9-11e9-b210-d663bd873d93"
                            , name =
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
                            , path = [ KMPathNode "aad436a7-c8a5-4237-a2bd-34decdf26a1f" ]
                            }
                in
                Expect.equal Nothing (getEventEntityVisibleName event)
        , test "get event entity visible name when changed" <|
            \_ ->
                Expect.equal (Just "Astronomy") (getEventEntityVisibleName editTagEvent)
        ]


deleteTagEvent : Event
deleteTagEvent =
    DeleteTagEvent
        { tagUuid = "1cf9c1f2-2df9-11e9-b210-d663bd873d93"
        }
        { uuid = "485bc170-2df3-11e9-b210-d663bd873d93"
        , path = [ KMPathNode "aad436a7-c8a5-4237-a2bd-34decdf26a1f" ]
        }


deleteTagEventTest : Test
deleteTagEventTest =
    describe "DeleteTagEvent"
        [ test "should encode and decode" <|
            \_ ->
                expectEventEncodeDecode deleteTagEvent
        , test "get event uuid" <|
            \_ ->
                Expect.equal "485bc170-2df3-11e9-b210-d663bd873d93" (getEventUuid deleteTagEvent)
        , test "get entity visible name" <|
            \_ ->
                Expect.equal Nothing (getEventEntityVisibleName deleteTagEvent)
        ]



{- question events -}


addQuestionEventTest : Test
addQuestionEventTest =
    describe "AddQuestionEvent"
        [ todo "test" ]


editQuestionEventTest : Test
editQuestionEventTest =
    describe "EditQuestionEvent"
        [ todo "test" ]


deleteQuestionEventTest : Test
deleteQuestionEventTest =
    describe "DeleteQuestionEvent"
        [ todo "test" ]



{- answer events -}


addAnswerEvent : Event
addAnswerEvent =
    AddAnswerEvent
        { answerUuid = "2bbe5372-2e00-11e9-b210-d663bd873d93"
        , label = "Yes"
        , advice = Just "Good choice"
        , metricMeasures =
            [ { metricUuid = "1ca4da0a-2e00-11e9-b210-d663bd873d93"
              , measure = 0.5
              , weight = 1
              }
            ]
        }
        { uuid = "efac9f6e-2e00-11e9-b210-d663bd873d93"
        , path =
            [ KMPathNode "aad436a7-c8a5-4237-a2bd-34decdf26a1f"
            , ChapterPathNode "42d0bd1e-2df3-11e9-b210-d663bd873d93"
            , QuestionPathNode "2f73c924-2dfc-11e9-b210-d663bd873d93"
            ]
        }


addAnswerEventTest : Test
addAnswerEventTest =
    describe "AddAnswerEvent"
        [ test "should encode and decode" <|
            \_ ->
                expectEventEncodeDecode addAnswerEvent
        , test "get event uuid" <|
            \_ ->
                Expect.equal "efac9f6e-2e00-11e9-b210-d663bd873d93" (getEventUuid addAnswerEvent)
        , test "get event entity visible name" <|
            \_ ->
                Expect.equal (Just "Yes") (getEventEntityVisibleName addAnswerEvent)
        ]


editAnswerEvent : Event
editAnswerEvent =
    EditAnswerEvent
        { answerUuid = "2bbe5372-2e00-11e9-b210-d663bd873d93"
        , label =
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
        , followUpIds =
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
        , path =
            [ KMPathNode "aad436a7-c8a5-4237-a2bd-34decdf26a1f"
            , ChapterPathNode "42d0bd1e-2df3-11e9-b210-d663bd873d93"
            , QuestionPathNode "2f73c924-2dfc-11e9-b210-d663bd873d93"
            ]
        }


editAnswerEventTest : Test
editAnswerEventTest =
    describe "EditAnswerEvent"
        [ test "should decode and encode" <|
            \_ ->
                expectEventEncodeDecode editAnswerEvent
        , test "get event uuid" <|
            \_ ->
                Expect.equal "efac9f6e-2e00-11e9-b210-d663bd873d93" (getEventUuid editAnswerEvent)
        , test "get entity visible name when not changed" <|
            \_ ->
                let
                    event =
                        EditAnswerEvent
                            { answerUuid = "2bbe5372-2e00-11e9-b210-d663bd873d93"
                            , label =
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
                            , followUpIds =
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
                            , path =
                                [ KMPathNode "aad436a7-c8a5-4237-a2bd-34decdf26a1f"
                                , ChapterPathNode "42d0bd1e-2df3-11e9-b210-d663bd873d93"
                                , QuestionPathNode "2f73c924-2dfc-11e9-b210-d663bd873d93"
                                ]
                            }
                in
                Expect.equal Nothing (getEventEntityVisibleName event)
        , test "get entity visible name when changed" <|
            \_ ->
                Expect.equal (Just "No") (getEventEntityVisibleName editAnswerEvent)
        ]


deleteAnswerEvent : Event
deleteAnswerEvent =
    DeleteAnswerEvent
        { answerUuid = "2bbe5372-2e00-11e9-b210-d663bd873d93"
        }
        { uuid = "efac9f6e-2e00-11e9-b210-d663bd873d93"
        , path =
            [ KMPathNode "aad436a7-c8a5-4237-a2bd-34decdf26a1f"
            , ChapterPathNode "42d0bd1e-2df3-11e9-b210-d663bd873d93"
            , QuestionPathNode "2f73c924-2dfc-11e9-b210-d663bd873d93"
            ]
        }


deleteAnswerEventTest : Test
deleteAnswerEventTest =
    describe "DeleteAnswerEvent"
        [ test "should encode and decode" <|
            \_ ->
                expectEventEncodeDecode deleteAnswerEvent
        , test "get event uuid" <|
            \_ ->
                Expect.equal "efac9f6e-2e00-11e9-b210-d663bd873d93" (getEventUuid deleteAnswerEvent)
        , test "get entity visible name" <|
            \_ ->
                Expect.equal Nothing (getEventEntityVisibleName deleteAnswerEvent)
        ]



{- reference events -}


addResourcePageReferenceEvent : Event
addResourcePageReferenceEvent =
    AddReferenceEvent
        (AddResourcePageReferenceEvent
            { referenceUuid = "3f52e8fc-2dfc-11e9-b210-d663bd873d93"
            , shortUuid = "atq"
            }
        )
        { uuid = "349624f6-2dfc-11e9-b210-d663bd873d93"
        , path =
            [ KMPathNode "aad436a7-c8a5-4237-a2bd-34decdf26a1f"
            , ChapterPathNode "42d0bd1e-2df3-11e9-b210-d663bd873d93"
            , QuestionPathNode "2f73c924-2dfc-11e9-b210-d663bd873d93"
            ]
        }


addURLReferenceEvent : Event
addURLReferenceEvent =
    AddReferenceEvent
        (AddURLReferenceEvent
            { referenceUuid = "e559cf36-2dfc-11e9-b210-d663bd873d93"
            , url = "http://example.com"
            , label = "Example"
            }
        )
        { uuid = "349624f6-2dfc-11e9-b210-d663bd873d93"
        , path =
            [ KMPathNode "aad436a7-c8a5-4237-a2bd-34decdf26a1f"
            , ChapterPathNode "42d0bd1e-2df3-11e9-b210-d663bd873d93"
            , QuestionPathNode "2f73c924-2dfc-11e9-b210-d663bd873d93"
            ]
        }


addCrossReferenceEvent : Event
addCrossReferenceEvent =
    AddReferenceEvent
        (AddCrossReferenceEvent
            { referenceUuid = "fe19113a-2dfc-11e9-b210-d663bd873d93"
            , targetUuid = "072af95a-2dfd-11e9-b210-d663bd873d93"
            , description = "Related"
            }
        )
        { uuid = "349624f6-2dfc-11e9-b210-d663bd873d93"
        , path =
            [ KMPathNode "aad436a7-c8a5-4237-a2bd-34decdf26a1f"
            , ChapterPathNode "42d0bd1e-2df3-11e9-b210-d663bd873d93"
            , QuestionPathNode "2f73c924-2dfc-11e9-b210-d663bd873d93"
            ]
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
                Expect.equal "349624f6-2dfc-11e9-b210-d663bd873d93" (getEventUuid event)
        , parametrized
            [ ( addResourcePageReferenceEvent, "atq" )
            , ( addURLReferenceEvent, "Example" )
            , ( addCrossReferenceEvent, "072af95a-2dfd-11e9-b210-d663bd873d93" )
            ]
            "get event entity visible name"
          <|
            \( event, name ) ->
                Expect.equal (Just name) (getEventEntityVisibleName event)
        ]


editResourcePageReferenceEvent : Event
editResourcePageReferenceEvent =
    EditReferenceEvent
        (EditResourcePageReferenceEvent
            { referenceUuid = "3f52e8fc-2dfc-11e9-b210-d663bd873d93"
            , shortUuid =
                { changed = True
                , value = Just "atq"
                }
            }
        )
        { uuid = "349624f6-2dfc-11e9-b210-d663bd873d93"
        , path =
            [ KMPathNode "aad436a7-c8a5-4237-a2bd-34decdf26a1f"
            , ChapterPathNode "42d0bd1e-2df3-11e9-b210-d663bd873d93"
            , QuestionPathNode "2f73c924-2dfc-11e9-b210-d663bd873d93"
            ]
        }


editURLReferenceEvent : Event
editURLReferenceEvent =
    EditReferenceEvent
        (EditURLReferenceEvent
            { referenceUuid = "e559cf36-2dfc-11e9-b210-d663bd873d93"
            , url =
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
        , path =
            [ KMPathNode "aad436a7-c8a5-4237-a2bd-34decdf26a1f"
            , ChapterPathNode "42d0bd1e-2df3-11e9-b210-d663bd873d93"
            , QuestionPathNode "2f73c924-2dfc-11e9-b210-d663bd873d93"
            ]
        }


editCrossReferenceEvent : Event
editCrossReferenceEvent =
    EditReferenceEvent
        (EditCrossReferenceEvent
            { referenceUuid = "fe19113a-2dfc-11e9-b210-d663bd873d93"
            , targetUuid =
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
        , path =
            [ KMPathNode "aad436a7-c8a5-4237-a2bd-34decdf26a1f"
            , ChapterPathNode "42d0bd1e-2df3-11e9-b210-d663bd873d93"
            , QuestionPathNode "2f73c924-2dfc-11e9-b210-d663bd873d93"
            ]
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
                Expect.equal "349624f6-2dfc-11e9-b210-d663bd873d93" (getEventUuid event)
        , parametrized
            [ ( editResourcePageReferenceEvent, Just "atq" )
            , ( editURLReferenceEvent, Just "Example" )
            , ( editCrossReferenceEvent, Nothing )
            ]
            "get event entity visible name"
          <|
            \( event, name ) ->
                Expect.equal name (getEventEntityVisibleName event)
        ]


deleteReferenceEvent : Event
deleteReferenceEvent =
    DeleteReferenceEvent
        { referenceUuid = "6606ebf8-2dff-11e9-b210-d663bd873d93"
        }
        { uuid = "349624f6-2dfc-11e9-b210-d663bd873d93"
        , path =
            [ KMPathNode "aad436a7-c8a5-4237-a2bd-34decdf26a1f"
            , ChapterPathNode "42d0bd1e-2df3-11e9-b210-d663bd873d93"
            , QuestionPathNode "2f73c924-2dfc-11e9-b210-d663bd873d93"
            ]
        }


deleteReferenceEventTest : Test
deleteReferenceEventTest =
    describe "DeleteReferenceEvent"
        [ test "should encode and decode" <|
            \_ ->
                expectEventEncodeDecode deleteReferenceEvent
        , test "get event uuid" <|
            \_ ->
                Expect.equal "349624f6-2dfc-11e9-b210-d663bd873d93" (getEventUuid deleteReferenceEvent)
        , test "get entity visible name" <|
            \_ ->
                Expect.equal Nothing (getEventEntityVisibleName deleteReferenceEvent)
        ]



{- expert events -}


addExpertEvent : Event
addExpertEvent =
    AddExpertEvent
        { expertUuid = "bad22d1c-2e01-11e9-b210-d663bd873d93"
        , name = "Albert Einstein"
        , email = "albert.einstein@example.com"
        }
        { uuid = "349624f6-2dfc-11e9-b210-d663bd873d93"
        , path =
            [ KMPathNode "aad436a7-c8a5-4237-a2bd-34decdf26a1f"
            , ChapterPathNode "42d0bd1e-2df3-11e9-b210-d663bd873d93"
            , QuestionPathNode "2f73c924-2dfc-11e9-b210-d663bd873d93"
            ]
        }


addExpertEventTest : Test
addExpertEventTest =
    describe "AddExpertEvent"
        [ test "should encode and decode" <|
            \_ -> expectEventEncodeDecode addExpertEvent
        , test "get event uuid" <|
            \_ ->
                Expect.equal "349624f6-2dfc-11e9-b210-d663bd873d93" (getEventUuid addExpertEvent)
        , test "get event entity visible name" <|
            \_ ->
                Expect.equal (Just "Albert Einstein") (getEventEntityVisibleName addExpertEvent)
        ]


editExpertEvent : Event
editExpertEvent =
    EditExpertEvent
        { expertUuid = "bad22d1c-2e01-11e9-b210-d663bd873d93"
        , name =
            { changed = True
            , value = Just "Nikola Tesla"
            }
        , email =
            { changed = False
            , value = Nothing
            }
        }
        { uuid = "349624f6-2dfc-11e9-b210-d663bd873d93"
        , path =
            [ KMPathNode "aad436a7-c8a5-4237-a2bd-34decdf26a1f"
            , ChapterPathNode "42d0bd1e-2df3-11e9-b210-d663bd873d93"
            , QuestionPathNode "2f73c924-2dfc-11e9-b210-d663bd873d93"
            ]
        }


editExpertEventTest : Test
editExpertEventTest =
    describe "EditExpertEvent"
        [ test "should encode and decode" <|
            \_ -> expectEventEncodeDecode editExpertEvent
        , test "get event uuid" <|
            \_ ->
                Expect.equal "349624f6-2dfc-11e9-b210-d663bd873d93" (getEventUuid editExpertEvent)
        , test "get event entity visible name when not changed" <|
            \_ ->
                let
                    event =
                        EditExpertEvent
                            { expertUuid = "1cf9c1f2-2df9-11e9-b210-d663bd873d93"
                            , name =
                                { changed = False
                                , value = Nothing
                                }
                            , email =
                                { changed = True
                                , value = Just "nikola.tesla@example.com"
                                }
                            }
                            { uuid = "485bc170-2df3-11e9-b210-d663bd873d93"
                            , path = [ KMPathNode "aad436a7-c8a5-4237-a2bd-34decdf26a1f" ]
                            }
                in
                Expect.equal Nothing (getEventEntityVisibleName event)
        , test "get event entity visible name when changed" <|
            \_ ->
                Expect.equal (Just "Nikola Tesla") (getEventEntityVisibleName editExpertEvent)
        ]


deleteExpertEvent : Event
deleteExpertEvent =
    DeleteExpertEvent
        { expertUuid = "bad22d1c-2e01-11e9-b210-d663bd873d93"
        }
        { uuid = "349624f6-2dfc-11e9-b210-d663bd873d93"
        , path =
            [ KMPathNode "aad436a7-c8a5-4237-a2bd-34decdf26a1f"
            , ChapterPathNode "42d0bd1e-2df3-11e9-b210-d663bd873d93"
            , QuestionPathNode "2f73c924-2dfc-11e9-b210-d663bd873d93"
            ]
        }


deleteExpertEventTest : Test
deleteExpertEventTest =
    describe "DeleteExpertEvent"
        [ test "should encode and decode" <|
            \_ ->
                expectEventEncodeDecode deleteExpertEvent
        , test "get event uuid" <|
            \_ ->
                Expect.equal "349624f6-2dfc-11e9-b210-d663bd873d93" (getEventUuid deleteExpertEvent)
        , test "get entity visible name" <|
            \_ ->
                Expect.equal Nothing (getEventEntityVisibleName deleteExpertEvent)
        ]



{- test utils -}


expectEventEncodeDecode : Event -> Expectation
expectEventEncodeDecode =
    expectEncodeDecode encodeEvent eventDecoder
