module Common.Pages.KMEditor.Editor.PreviewTest exposing (suite)

{-| Opening the preview on the question being edited.

A question deep in a knowledge model is not reachable by scrolling: it is behind
the answer that reveals it, inside the list item that repeats it. So the preview
answers its way down to the question first, and then goes there.

What is checked here is that chain — the question the editor is open on, the
replies fabricated to reach it, and the path they add up to — against the same
shape the move dialog is checked against: a chapter, an options question in it,
that question's answer, and a follow-up question under the answer.

The rule worth pinning is the last one. Preview values are saved on the editor
and shared by everybody in it, so reaching a question must not quietly rewrite
somebody's answers: a saved reply that already leads there is used as it stands.

-}

import Common.Api.Models.Event exposing (Event)
import Common.Api.Models.Event.AddQuestionEventData as AddQuestionEventData
import Common.Api.Models.Event.AddReferenceEventData as AddReferenceEventData
import Common.Api.Models.Event.EventContent exposing (EventContent(..))
import Common.Api.Models.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Common.Api.Models.KnowledgeModelEditor.KnowledgeModelEditorState exposing (KnowledgeModelEditorState(..))
import Common.Api.Models.KnowledgeModelPackage as KnowledgeModelPackage
import Common.Api.Models.ProjectDetail.Reply exposing (Reply)
import Common.Api.Models.ProjectDetail.Reply.ReplyValue as ReplyValue
import Common.Api.Models.ProjectQuestionnaire as ProjectQuestionnaire exposing (ProjectQuestionnaire)
import Common.Pages.KMEditor.Editor.EditorContext as EditorContext exposing (EditorContext)
import Dict
import Expect
import Random
import Test exposing (Test, describe, test)
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


questionUuid : String
questionUuid =
    "00000000-0000-4000-8000-00000000000q"


answerUuid : String
answerUuid =
    "00000000-0000-4000-8000-00000000000a"


otherAnswerUuid : String
otherAnswerUuid =
    "00000000-0000-4000-8000-00000000000b"


followUpUuid : String
followUpUuid =
    "00000000-0000-4000-8000-00000000000f"


referenceUuid : String
referenceUuid =
    "00000000-0000-4000-8000-00000000000r"


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
    , event questionUuid
        otherAnswerUuid
        (AddAnswerEvent { label = "Other answer", advice = Nothing, metricMeasures = [], annotations = [] })
    , event answerUuid
        followUpUuid
        (AddQuestionEvent
            (AddQuestionEventData.AddQuestionOptionsEvent
                { title = "Follow-up", text = Nothing, requiredPhaseUuid = Nothing, tagUuids = [], annotations = [] }
            )
        )
    , event followUpUuid
        referenceUuid
        (AddReferenceEvent
            (AddReferenceEventData.AddReferenceURLEvent
                { url = "https://example.org", label = "A reference", annotations = [] }
            )
        )
    ]


editorContextWith : Dict.Dict String Reply -> Maybe String -> EditorContext
editorContextWith replies mbActiveUuid =
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
        , replies = replies
        , workspaceUuid = Nothing
        }
        (Maybe.map Uuid.fromUuidString mbActiveUuid)


editorContext : EditorContext
editorContext =
    editorContextWith Dict.empty Nothing



-- THE PREVIEW IT OPENS


{-| What the preview does on entry: answer the way down to the question the
editor is open on, and work out the path that leads there.
-}
openOn : Dict.Dict String Reply -> String -> ( Maybe String, ProjectQuestionnaire )
openOn saved activeUuid =
    let
        context =
            editorContextWith saved (Just activeUuid)

        knowledgeModel =
            EditorContext.getFilteredKM context

        questionnaire =
            ProjectQuestionnaire.createQuestionnaireDetail KnowledgeModelPackage.dummy knowledgeModel

        ( _, _, generated ) =
            ProjectQuestionnaire.generateReplies
                (Time.millisToPosix 0)
                (Random.initialSeed 0)
                (EditorContext.getActiveQuestionUuid context)
                knowledgeModel
                { questionnaire | replies = saved }
    in
    ( ProjectQuestionnaire.getClosestQuestionParentPath generated
        (KnowledgeModel.createParentMap knowledgeModel)
        (EditorContext.getActiveQuestionUuid context)
    , generated
    )


answeredWith : String -> Reply
answeredWith uuid =
    { value = ReplyValue.AnswerReply uuid
    , createdAt = Time.millisToPosix 0
    , createdBy = Nothing
    }


suite : Test
suite =
    describe "Common.Pages.KMEditor.Editor preview"
        [ describe "the question the editor is open on"
            [ test "is the question itself" <|
                \_ ->
                    editorContextWith Dict.empty (Just followUpUuid)
                        |> EditorContext.getActiveQuestionUuid
                        |> Expect.equal followUpUuid
            , test "is the question an answer belongs to" <|
                \_ ->
                    editorContextWith Dict.empty (Just answerUuid)
                        |> EditorContext.getActiveQuestionUuid
                        |> Expect.equal questionUuid
            , test "is the question a reference belongs to" <|
                \_ ->
                    editorContextWith Dict.empty (Just referenceUuid)
                        |> EditorContext.getActiveQuestionUuid
                        |> Expect.equal followUpUuid
            , test "is nothing at all on a chapter — the preview opens on it instead" <|
                \_ ->
                    editorContextWith Dict.empty (Just chapterUuid)
                        |> EditorContext.getActiveQuestionUuid
                        |> Expect.equal ""
            , test "is nothing at all on the knowledge model itself" <|
                \_ ->
                    EditorContext.getActiveQuestionUuid editorContext
                        |> Expect.equal ""
            ]
        , describe "the chapter something sits in"
            [ test "is found from a question deep inside it" <|
                \_ ->
                    EditorContext.getChapterUuid followUpUuid editorContext
                        |> Expect.equal chapterUuid
            , test "is itself" <|
                \_ ->
                    EditorContext.getChapterUuid chapterUuid editorContext
                        |> Expect.equal chapterUuid
            ]
        , describe "answering the way down to a question"
            [ test "picks the answer that reveals it" <|
                \_ ->
                    openOn Dict.empty followUpUuid
                        |> Tuple.second
                        |> .replies
                        |> Dict.get (chapterUuid ++ "." ++ questionUuid)
                        |> Maybe.map (.value >> ReplyValue.getAnswerUuid)
                        |> Expect.equal (Just answerUuid)
            , test "and leads to the question's own path" <|
                \_ ->
                    openOn Dict.empty followUpUuid
                        |> Tuple.first
                        |> Expect.equal
                            (Just (String.join "." [ chapterUuid, questionUuid, answerUuid, followUpUuid ]))
            , test "answers nothing at all for a question already in the open" <|
                \_ ->
                    openOn Dict.empty questionUuid
                        |> Tuple.second
                        |> .replies
                        |> Dict.isEmpty
                        |> Expect.equal True
            , test "and still leads to it" <|
                \_ ->
                    openOn Dict.empty questionUuid
                        |> Tuple.first
                        |> Expect.equal (Just (chapterUuid ++ "." ++ questionUuid))
            ]
        , describe "the values already saved"
            [ test "are kept when they already lead to the question" <|
                \_ ->
                    let
                        saved =
                            Dict.fromList [ ( chapterUuid ++ "." ++ questionUuid, answeredWith answerUuid ) ]
                    in
                    openOn saved followUpUuid
                        |> Tuple.second
                        |> .replies
                        |> Expect.equal saved
            , test "give way only where they lead somewhere else" <|
                \_ ->
                    let
                        saved =
                            Dict.fromList [ ( chapterUuid ++ "." ++ questionUuid, answeredWith otherAnswerUuid ) ]
                    in
                    openOn saved followUpUuid
                        |> Tuple.second
                        |> .replies
                        |> Dict.get (chapterUuid ++ "." ++ questionUuid)
                        |> Maybe.map (.value >> ReplyValue.getAnswerUuid)
                        |> Expect.equal (Just answerUuid)
            , test "are left alone entirely when no question is open" <|
                \_ ->
                    let
                        saved =
                            Dict.fromList [ ( chapterUuid ++ "." ++ questionUuid, answeredWith otherAnswerUuid ) ]
                    in
                    openOn saved chapterUuid
                        |> Tuple.second
                        |> .replies
                        |> Expect.equal saved
            ]
        ]
