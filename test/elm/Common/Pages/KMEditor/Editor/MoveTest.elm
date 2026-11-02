module Common.Pages.KMEditor.Editor.MoveTest exposing (suite)

{-| Where an entity may be moved to.

The rules are the whole of the move dialog, and getting one wrong loses a branch
of the knowledge model, so they are checked against a knowledge model built here
rather than against the tree they are drawn as: a chapter with an options
question in it, that question's answer, and a follow-up question under the
answer.

-}

import Common.Api.Models.Event exposing (Event)
import Common.Api.Models.Event.AddQuestionEventData as AddQuestionEventData
import Common.Api.Models.Event.EventContent exposing (EventContent(..))
import Common.Api.Models.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Common.Api.Models.KnowledgeModelEditor.KnowledgeModelEditorState exposing (KnowledgeModelEditorState(..))
import Common.Pages.KMEditor.Editor.EditorContext as EditorContext exposing (EditorContext)
import Common.Pages.KMEditor.Editor.Move as Move
import Dict
import Expect
import Gettext
import Html.Attributes as Attr
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (attribute, class, text)
import Time
import Uuid
import Version



-- THE KNOWLEDGE MODEL


kmUuid : String
kmUuid =
    "00000000-0000-4000-8000-000000000000"


chapterUuid : String
chapterUuid =
    "00000000-0000-4000-8000-00000000000c"


otherChapterUuid : String
otherChapterUuid =
    "00000000-0000-4000-8000-00000000000d"


questionUuid : String
questionUuid =
    "00000000-0000-4000-8000-00000000000q"


answerUuid : String
answerUuid =
    "00000000-0000-4000-8000-00000000000a"


followUpUuid : String
followUpUuid =
    "00000000-0000-4000-8000-00000000000f"


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


event : String -> String -> EventContent -> Event
event parentUuid entityUuid content =
    { uuid = "event-" ++ entityUuid
    , parentUuid = parentUuid
    , entityUuid = entityUuid
    , content = content
    , createdAt = Time.millisToPosix 0
    }


events : List Event
events =
    [ event kmUuid chapterUuid (AddChapterEvent { title = "Chapter", text = Nothing, annotations = [] })
    , event kmUuid otherChapterUuid (AddChapterEvent { title = "Other chapter", text = Nothing, annotations = [] })
    , event chapterUuid
        questionUuid
        (AddQuestionEvent
            (AddQuestionEventData.AddQuestionOptionsEvent
                { title = "Question", text = Nothing, requiredPhaseUuid = Nothing, tagUuids = [], annotations = [] }
            )
        )
    , event questionUuid
        answerUuid
        (AddAnswerEvent { label = "Answer", advice = Nothing, metricMeasures = [], annotations = [] })
    , event answerUuid
        followUpUuid
        (AddQuestionEvent
            (AddQuestionEventData.AddQuestionOptionsEvent
                { title = "Follow-up", text = Nothing, requiredPhaseUuid = Nothing, tagUuids = [], annotations = [] }
            )
        )
    ]


editorContext : EditorContext
editorContext =
    EditorContext.init
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
        Nothing



-- THE TREE IT DRAWS


{-| The tree's messages are not what is under test here — where it lets you go
is — so they carry nothing.
-}
type Msg
    = Selected
    | Toggled


{-| The whole tree open, so every place it could go is in the output.
-}
tree : Move.MovingEntity -> String -> Query.Single Msg
tree entity uuid =
    Query.fromHtml
        (Move.tree
            { locale = Gettext.defaultLocale, onSelect = always Selected, onToggle = \_ _ -> Toggled }
            editorContext
            (Move.expandAll editorContext (Move.open entity uuid editorContext))
        )


{-| The whole knowledge model is drawn whatever is moving — six entries, here —
because a place is found by recognizing what is around it. How many of them can
be chosen is the rule.
-}
countOf : Selector.Selector -> Move.MovingEntity -> String -> Int -> Expect.Expectation
countOf selector entity uuid expected =
    tree entity uuid
        |> Query.findAll [ class "ui-tree__entry", selector ]
        |> Query.count (Expect.equal expected)


suite : Test
suite =
    describe "Common.Pages.KMEditor.Editor.Move"
        [ describe "what moves at all"
            [ test "a question does" <|
                \_ -> Expect.equal (Just Move.MovingQuestion) (Move.movingEntity questionUuid editorContext)
            , test "an answer does" <|
                \_ -> Expect.equal (Just Move.MovingAnswer) (Move.movingEntity answerUuid editorContext)
            , test "a chapter does not — it lives in one place and is ordered there" <|
                \_ -> Expect.equal Nothing (Move.movingEntity chapterUuid editorContext)
            , test "the knowledge model itself does not" <|
                \_ -> Expect.equal Nothing (Move.movingEntity kmUuid editorContext)
            ]
        , describe "nothing is chosen until something is"
            [ test "the dialog opens with no target" <|
                \_ ->
                    Move.open Move.MovingQuestion questionUuid editorContext
                        |> Move.canMove
                        |> Expect.equal False
            , test "and has one once a place is picked" <|
                \_ ->
                    Move.open Move.MovingQuestion questionUuid editorContext
                        |> Move.setSelected otherChapterUuid
                        |> Move.canMove
                        |> Expect.equal True
            , test "the event carries where it goes" <|
                \_ ->
                    Move.open Move.MovingQuestion questionUuid editorContext
                        |> Move.setSelected otherChapterUuid
                        |> Move.event
                        |> Expect.equal (Just (MoveQuestionEvent { targetUuid = otherChapterUuid }))
            ]
        , describe "where a question may go"
            [ test "the whole knowledge model is drawn, whatever can take it" <|
                \_ ->
                    tree Move.MovingQuestion questionUuid
                        |> Query.findAll [ class "ui-tree__entry" ]
                        |> Query.count (Expect.equal 6)
            , test "only the other chapter takes it: not itself, not what is inside it, not the chapter it is already in" <|
                \_ -> countOf (attribute (Attr.disabled True)) Move.MovingQuestion questionUuid 5
            , test "and that one is the other chapter" <|
                \_ ->
                    tree Move.MovingQuestion questionUuid
                        |> Query.find [ class "ui-tree__entry", attribute (Attr.disabled False) ]
                        |> Query.has [ text "Other chapter" ]
            ]
        , describe "where an answer may go"
            [ test "nowhere here: the one options question is the one it is in already" <|
                \_ -> countOf (attribute (Attr.disabled True)) Move.MovingAnswer answerUuid 6
            ]
        , describe "the entity being moved"
            [ test "is in the tree, marked" <|
                \_ -> tree Move.MovingQuestion questionUuid |> Query.has [ text "Moving" ]
            ]
        ]
