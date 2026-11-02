module Common.Pages.KMEditor.Editor.EditorContextTest exposing (suite)

{-| The editor's store, folded from a history of events.

Everything here is pure: a knowledge model editor built in the test, a list of
events, and what the store says afterwards. No server, no socket, no browser.

-}

import Common.Api.Models.Event exposing (Event)
import Common.Api.Models.Event.AddIntegrationEventData as AddIntegrationEventData
import Common.Api.Models.Event.AddQuestionEventData as AddQuestionEventData
import Common.Api.Models.Event.EventContent exposing (EventContent(..))
import Common.Api.Models.Event.EventField as EventField
import Common.Api.Models.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Common.Api.Models.KnowledgeModelEditor.KnowledgeModelEditorState exposing (KnowledgeModelEditorState(..))
import Common.Api.Models.KnowledgeModelEditorDetail exposing (KnowledgeModelEditorDetail)
import Common.Api.Models.ProjectDetail.Reply exposing (Reply)
import Common.Api.Models.ProjectDetail.Reply.ReplyValue exposing (ReplyValue(..))
import Common.Pages.KMEditor.Editor.EditorContext as EditorContext exposing (EditorContext)
import Dict
import Expect
import Gettext
import Test exposing (Test, describe, test)
import Time
import Uuid
import Version



-- A KNOWLEDGE MODEL EDITOR WITH NOTHING IN IT


kmUuid : String
kmUuid =
    "00000000-0000-4000-8000-000000000000"


emptyKm : KnowledgeModel
emptyKm =
    { uuid = Uuid.fromUuidString kmUuid
    , chapterUuids = []
    , tagUuids = []
    , integrationUuids = []
    , metricUuids = []
    , phaseUuids = []
    , resourceCollectionUuids = []
    , entities = KnowledgeModel.empty.entities
    , annotations = []
    }


editor : List Event -> KnowledgeModelEditorDetail
editor events =
    { uuid = Uuid.fromUuidString "11111111-1111-4111-8111-111111111111"
    , name = "Editor"
    , description = ""
    , id = "editor"
    , language = "en"
    , license = ""
    , readme = ""
    , version = Version.create 1 0 0
    , knowledgeModel = emptyKm
    , forkOfPackage = Nothing
    , previousPackage = Nothing
    , events = events
    , state = Edited
    , replies = Dict.empty
    , workspaceUuid = Nothing
    }


context : List Event -> EditorContext
context events =
    EditorContext.init (editor events) Nothing



-- EVENTS


event : String -> String -> EventContent -> Event
event parentUuid entityUuid content =
    { uuid = "event-" ++ entityUuid
    , parentUuid = parentUuid
    , entityUuid = entityUuid
    , content = content
    , createdAt = Time.millisToPosix 0
    }


addChapter : String -> String -> Event
addChapter uuid title =
    event kmUuid uuid (AddChapterEvent { title = title, text = Nothing, annotations = [] })


editChapterTitle : String -> String -> Event
editChapterTitle uuid title =
    event kmUuid
        uuid
        (EditChapterEvent
            { title = EventField.create title True
            , text = EventField.empty
            , questionUuids = EventField.empty
            , annotations = EventField.empty
            }
        )


deleteChapter : String -> Event
deleteChapter uuid =
    event kmUuid uuid DeleteChapterEvent


addQuestion : String -> String -> String -> Event
addQuestion parentUuid uuid title =
    event parentUuid
        uuid
        (AddQuestionEvent
            (AddQuestionEventData.AddQuestionOptionsEvent
                { title = title
                , text = Nothing
                , requiredPhaseUuid = Nothing
                , tagUuids = []
                , annotations = []
                }
            )
        )


addAnswer : String -> String -> String -> Event
addAnswer parentUuid uuid label =
    event parentUuid
        uuid
        (AddAnswerEvent { label = label, advice = Nothing, metricMeasures = [], annotations = [] })


addIntegration : String -> Event
addIntegration uuid =
    event kmUuid uuid (AddIntegrationEvent AddIntegrationEventData.init)


reply : Reply
reply =
    { value = StringReply "", createdAt = Time.millisToPosix 0, createdBy = Nothing }


deleteQuestion : String -> String -> Event
deleteQuestion parentUuid uuid =
    event parentUuid uuid DeleteQuestionEvent


chapterTitles : EditorContext -> List String
chapterTitles editorContext =
    List.map .title (KnowledgeModel.getChapters (EditorContext.getFilteredKM editorContext))


suite : Test
suite =
    describe "Common.Pages.KMEditor.Editor.EditorContext"
        [ describe "folding the history"
            [ test "an empty history leaves the knowledge model as the server sent it" <|
                \_ ->
                    chapterTitles (context []) |> Expect.equal []
            , test "an add event puts the entity in the knowledge model" <|
                \_ ->
                    chapterTitles (context [ addChapter "ch1" "First" ])
                        |> Expect.equal [ "First" ]
            , test "an edit event patches only the fields it carries" <|
                \_ ->
                    chapterTitles (context [ addChapter "ch1" "First", editChapterTitle "ch1" "Renamed" ])
                        |> Expect.equal [ "Renamed" ]
            , test "an edit event for an entity that is not there changes nothing" <|
                \_ ->
                    chapterTitles (context [ editChapterTitle "ch1" "Renamed" ])
                        |> Expect.equal []
            , test "the events are kept, in the order they were applied" <|
                \_ ->
                    (context [ addChapter "ch1" "First", editChapterTitle "ch1" "Renamed" ]).kmEditor.events
                        |> List.map .entityUuid
                        |> Expect.equal [ "ch1", "ch1" ]
            ]
        , describe "what has been done to each entity"
            [ test "an added entity is added, not edited" <|
                \_ ->
                    let
                        editorContext =
                            context [ addChapter "ch1" "First" ]
                    in
                    ( EditorContext.isAdded "ch1" editorContext, EditorContext.isEdited "ch1" editorContext )
                        |> Expect.equal ( True, False )
            , test "an entity added and then edited stays added" <|
                \_ ->
                    let
                        editorContext =
                            context [ addChapter "ch1" "First", editChapterTitle "ch1" "Renamed" ]
                    in
                    ( EditorContext.isAdded "ch1" editorContext, EditorContext.isEdited "ch1" editorContext )
                        |> Expect.equal ( True, False )
            , test "an entity that was only edited is edited" <|
                \_ ->
                    let
                        editorContext =
                            EditorContext.applyEvent False
                                (editChapterTitle "ch1" "Renamed")
                                (context [ addChapter "ch1" "First" ] |> clearAdded)
                    in
                    EditorContext.isEdited "ch1" editorContext |> Expect.equal True
            , test "a deleted entity is neither added nor edited any more" <|
                \_ ->
                    let
                        editorContext =
                            context [ addChapter "ch1" "First", deleteChapter "ch1" ]
                    in
                    [ EditorContext.isAdded "ch1" editorContext
                    , EditorContext.isEdited "ch1" editorContext
                    , EditorContext.isDeleted "ch1" editorContext
                    ]
                        |> Expect.equal [ False, False, True ]
            , test "added and then deleted means it was never there" <|
                \_ ->
                    EditorContext.isAddedAndDeleted "ch1" (context [ addChapter "ch1" "First", deleteChapter "ch1" ])
                        |> Expect.equal True
            ]
        , describe "delete is soft"
            [ test "a deleted chapter is gone from the filtered knowledge model" <|
                \_ ->
                    chapterTitles (context [ addChapter "ch1" "First", addChapter "ch2" "Second", deleteChapter "ch1" ])
                        |> Expect.equal [ "Second" ]
            , test "but it is still in the dictionaries, so the tree can strike it through" <|
                \_ ->
                    let
                        editorContext =
                            context [ addChapter "ch1" "First", deleteChapter "ch1" ]
                    in
                    KnowledgeModel.getChapter "ch1" editorContext.kmEditor.knowledgeModel
                        |> Maybe.map .title
                        |> Expect.equal (Just "First")
            , test "a deleted question is gone from its chapter" <|
                \_ ->
                    let
                        editorContext =
                            context
                                [ addChapter "ch1" "First"
                                , addQuestion "ch1" "q1" "Kept"
                                , addQuestion "ch1" "q2" "Going"
                                , deleteQuestion "ch1" "q2"
                                ]
                    in
                    KnowledgeModel.getChapterQuestions "ch1" (EditorContext.getFilteredKM editorContext)
                        |> List.length
                        |> Expect.equal 1
            , test "a deleted question takes its answers out of reach" <|
                \_ ->
                    let
                        editorContext =
                            context
                                [ addChapter "ch1" "First"
                                , addQuestion "ch1" "q1" "Question"
                                , addAnswer "q1" "a1" "Yes"
                                , deleteQuestion "ch1" "q1"
                                ]
                    in
                    EditorContext.isReachable editorContext "a1" |> Expect.equal False
            ]
        , describe "where the editor is"
            [ test "an entity added locally becomes the active one" <|
                \_ ->
                    EditorContext.applyEvent True (addChapter "ch1" "First") (context [])
                        |> .activeUuid
                        |> Expect.equal "ch1"
            , test "an entity added by somebody else does not steal the cursor" <|
                \_ ->
                    EditorContext.applyEvent False (addChapter "ch1" "First") (context [])
                        |> .activeUuid
                        |> Expect.equal kmUuid
            , test "deleting the active entity moves the editor up to its parent" <|
                \_ ->
                    let
                        opened =
                            EditorContext.setActiveEditor (Just "q1")
                                (context [ addChapter "ch1" "First", addQuestion "ch1" "q1" "Question" ])
                    in
                    EditorContext.applyEvent False (deleteQuestion "ch1" "q1") opened
                        |> .activeUuid
                        |> Expect.equal "ch1"
            , test "moving to an entity opens every ancestor on the way" <|
                \_ ->
                    let
                        editorContext =
                            EditorContext.setActiveEditor (Just "a1")
                                (context
                                    [ addChapter "ch1" "First"
                                    , addQuestion "ch1" "q1" "Question"
                                    , addAnswer "q1" "a1" "Yes"
                                    ]
                                )
                    in
                    [ EditorContext.isNodeOpen "q1" editorContext
                    , EditorContext.isNodeOpen "ch1" editorContext
                    , EditorContext.isNodeOpen kmUuid editorContext
                    ]
                        |> Expect.equal [ True, True, True ]
            , test "collapse all closes everything and expand all opens it again" <|
                \_ ->
                    let
                        editorContext =
                            context [ addChapter "ch1" "First", addQuestion "ch1" "q1" "Question" ]
                    in
                    ( List.length (EditorContext.treeCollapseAll editorContext).openNodeUuids
                    , List.length (EditorContext.treeExpandAll editorContext).openNodeUuids
                    )
                        |> Expect.equal ( 0, 3 )
            ]
        , describe "reading the store"
            [ test "the knowledge model itself is not a child of anything, so its route names no entity" <|
                \_ ->
                    EditorContext.getEditUuid kmUuid (context []) |> Expect.equal Nothing
            , test "an entity's route names it" <|
                \_ ->
                    EditorContext.getEditUuid "ch1" (context [ addChapter "ch1" "First" ])
                        |> Maybe.map Uuid.toString
                        |> Expect.equal (Just "ch1")
            , test "an added entity knows its parent" <|
                \_ ->
                    EditorContext.getParentUuid "q1" (context [ addChapter "ch1" "First", addQuestion "ch1" "q1" "Question" ])
                        |> Expect.equal "ch1"
            , test "filterDeleted drops both the deleted and the unknown" <|
                \_ ->
                    EditorContext.filterDeleted
                        (context [ addChapter "ch1" "First", addChapter "ch2" "Second", deleteChapter "ch2" ])
                        [ "ch1", "ch2", "not-an-entity" ]
                        |> Expect.equal [ "ch1" ]
            , test "sortDeleted puts what is going after what is staying" <|
                \_ ->
                    EditorContext.sortDeleted identity
                        (context [ addChapter "ch1" "First", addChapter "ch2" "Second", deleteChapter "ch1" ])
                        [ "ch1", "ch2" ]
                        |> Expect.equal [ "ch2", "ch1" ]
            , test "every entity is in getAllUuids, with the knowledge model itself" <|
                \_ ->
                    EditorContext.getAllUuids (context [ addChapter "ch1" "First", addQuestion "ch1" "q1" "Question" ])
                        |> List.sort
                        |> Expect.equal (List.sort [ kmUuid, "ch1", "q1" ])
            , test "the active entity is the one setActiveEditor was given" <|
                \_ ->
                    EditorContext.isActive "ch1"
                        (EditorContext.setActiveEditor (Just "ch1") (context [ addChapter "ch1" "First" ]))
                        |> Expect.equal True
            , test "a node can be closed and opened again" <|
                \_ ->
                    let
                        editorContext =
                            context [ addChapter "ch1" "First" ]

                        closed =
                            EditorContext.setNodeOpen kmUuid False editorContext
                    in
                    ( EditorContext.isNodeOpen kmUuid closed
                    , EditorContext.isNodeOpen kmUuid (EditorContext.setNodeOpen kmUuid True closed)
                    )
                        |> Expect.equal ( False, True )
            , test "the preview's replies are kept on the editor" <|
                \_ ->
                    EditorContext.setReplies (Dict.singleton "path" reply) (context [])
                        |> .kmEditor
                        |> .replies
                        |> Dict.keys
                        |> Expect.equal [ "path" ]
            ]
        , describe "an integration with no type yet"
            [ test "a newly added integration has no type until one is chosen" <|
                \_ ->
                    EditorContext.isEmptyIntegrationEditor "i1" (context [ addIntegration "i1" ])
                        |> Expect.equal True
            , test "choosing a type takes it off the list" <|
                \_ ->
                    EditorContext.isEmptyIntegrationEditor "i1"
                        (EditorContext.removeEmptyIntegrationEditor "i1" (context [ addIntegration "i1" ]))
                        |> Expect.equal False
            ]
        , describe "warnings"
            [ test "a chapter with no title is a warning against that chapter" <|
                \_ ->
                    warningsOf (context [ addChapter "ch1" "" ])
                        |> Expect.equal [ ( "ch1", "Empty title for chapter" ) ]
            , test "an options question with no answers is a warning" <|
                \_ ->
                    warningsOf (context [ addChapter "ch1" "First", addQuestion "ch1" "q1" "Question" ])
                        |> Expect.equal [ ( "q1", "No answers for options question" ) ]
            , test "a deleted entity raises nothing" <|
                \_ ->
                    warningsOf (context [ addChapter "ch1" "", deleteChapter "ch1" ])
                        |> Expect.equal []
            , test "a finished knowledge model has nothing to say" <|
                \_ ->
                    warningsOf
                        (context
                            [ addChapter "ch1" "First"
                            , addQuestion "ch1" "q1" "Question"
                            , addAnswer "q1" "a1" "Yes"
                            ]
                        )
                        |> Expect.equal []
            ]
        ]


warningsOf : EditorContext -> List ( String, String )
warningsOf editorContext =
    EditorContext.computeWarnings { locale = Gettext.defaultLocale, builtIns = [], secrets = [] } editorContext
        |> .warnings
        |> List.map (\warning -> ( warning.editorUuid, warning.message ))


{-| The store with nothing marked as added, which is how an entity that came
with the published knowledge model behaves.
-}
clearAdded : EditorContext -> EditorContext
clearAdded editorContext =
    { editorContext | addedUuids = [] }
