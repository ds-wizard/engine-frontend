module Common.Components.Questionnaire.RebuildTest exposing (derivedTest, needsRebuildTest, unansweredTest)

import Common.Api.Models.ProjectDetail.ProjectEvent as ProjectEvent exposing (ProjectEvent)
import Common.Api.Models.ProjectDetail.Reply exposing (Reply)
import Common.Api.Models.ProjectDetail.Reply.ReplyValue exposing (ReplyValue(..))
import Common.Api.Models.ProjectQuestionnaire as ProjectQuestionnaire exposing (ProjectQuestionnaire)
import Common.Components.Questionnaire.Fixture as Fixture
import Common.Components.Questionnaire.Rebuild as Rebuild
import Dict exposing (Dict)
import Expect
import Test exposing (Test, describe, test)
import Time
import Uuid


value1Path : String
value1Path =
    Fixture.chapter1 ++ "." ++ Fixture.value1


setReply : String -> ReplyValue -> ProjectEvent
setReply path value =
    ProjectEvent.SetReply { uuid = Uuid.nil, path = path, value = value, createdAt = Time.millisToPosix 0, createdBy = Nothing }


clearReply : String -> ProjectEvent
clearReply path =
    ProjectEvent.ClearReply { uuid = Uuid.nil, path = path, createdAt = Time.millisToPosix 0, createdBy = Nothing }


withValue1 : Maybe String -> ProjectQuestionnaire
withValue1 mbValue =
    Fixture.questionnaire
        (case mbValue of
            Just value ->
                Dict.singleton value1Path (Fixture.reply (StringReply value))

            Nothing ->
                Dict.empty
        )


{-| Counts that are deliberately wrong. A chapter the event should not recount
keeps its wrong number, which is how the tests see what was not counted again —
Elm has no way to ask whether two values are the same reference.
-}
stale : Dict String Int
stale =
    Dict.fromList [ ( Fixture.chapter1, 42 ), ( Fixture.chapter2, 99 ) ]


update : ProjectEvent -> ProjectQuestionnaire -> ProjectQuestionnaire -> Dict String Int
update event before after =
    Rebuild.updateChapterUnansweredCounts event { before = before, after = after } stale


unansweredTest : Test
unansweredTest =
    describe "Rebuild.updateChapterUnansweredCounts"
        [ test "does not count again while an answered value is typed into" <|
            \_ ->
                update (setReply value1Path (StringReply "ab")) (withValue1 (Just "a")) (withValue1 (Just "ab"))
                    |> Expect.equal stale
        , test "counts again the chapter a value became answered in, and only that chapter" <|
            \_ ->
                let
                    after =
                        withValue1 (Just "a")
                in
                update (setReply value1Path (StringReply "a")) (withValue1 Nothing) after
                    |> Expect.equal
                        (Dict.fromList
                            [ ( Fixture.chapter1, ProjectQuestionnaire.calculateUnansweredQuestionsForChapter after Fixture.chapter1 )
                            , ( Fixture.chapter2, 99 )
                            ]
                        )
        , test "counts again when a value is emptied" <|
            \_ ->
                let
                    after =
                        withValue1 (Just "")
                in
                update (setReply value1Path (StringReply "")) (withValue1 (Just "a")) after
                    |> Dict.get Fixture.chapter1
                    |> Expect.equal (Just (ProjectQuestionnaire.calculateUnansweredQuestionsForChapter after Fixture.chapter1))
        , test "counts again the chapter of a cleared reply" <|
            \_ ->
                let
                    after =
                        withValue1 Nothing
                in
                update (clearReply value1Path) (withValue1 (Just "a")) after
                    |> Expect.equal
                        (Dict.fromList
                            [ ( Fixture.chapter1, ProjectQuestionnaire.calculateUnansweredQuestionsForChapter after Fixture.chapter1 )
                            , ( Fixture.chapter2, 99 )
                            ]
                        )
        , test "counts every chapter again when the phase changes" <|
            \_ ->
                let
                    questionnaire =
                        withValue1 Nothing
                in
                update (ProjectEvent.SetPhase { uuid = Uuid.nil, phaseUuid = Nothing, createdAt = Time.millisToPosix 0, createdBy = Nothing }) questionnaire questionnaire
                    |> Expect.equal (Rebuild.chapterUnansweredCounts questionnaire)
        , test "starts from a count for every chapter" <|
            \_ ->
                Rebuild.chapterUnansweredCounts (withValue1 Nothing)
                    |> Dict.keys
                    |> List.sort
                    |> Expect.equal (List.sort [ Fixture.chapter1, Fixture.chapter2 ])
        ]


needsRebuildTest : Test
needsRebuildTest =
    let
        reply : Reply
        reply =
            Fixture.reply (StringReply "")
    in
    describe "Rebuild.needsRebuild"
        [ test "rebuilds when an answer is chosen, a list's items change, or a reply is cleared" <|
            \_ ->
                [ setReply value1Path (AnswerReply Fixture.optionsAnswer)
                , setReply value1Path (ItemListReply [])
                , clearReply value1Path
                ]
                    |> List.map Rebuild.needsRebuild
                    |> Expect.equal [ True, True, True ]
        , test "does not rebuild for typing, choices, labels or the phase" <|
            \_ ->
                [ setReply value1Path reply.value
                , setReply value1Path (MultiChoiceReply [])
                , ProjectEvent.SetLabels { uuid = Uuid.nil, path = value1Path, value = [], createdAt = Time.millisToPosix 0, createdBy = Nothing }
                , ProjectEvent.SetPhase { uuid = Uuid.nil, phaseUuid = Nothing, createdAt = Time.millisToPosix 0, createdBy = Nothing }
                ]
                    |> List.map Rebuild.needsRebuild
                    |> Expect.equal [ False, False, False, False ]
        ]


{-| The toolbar's other numbers: TODOs follow the labels, and warnings are the
same list after a keystroke that brought no warning and took none away.
-}
derivedTest : Test
derivedTest =
    describe "Rebuild, the toolbar's numbers"
        [ test "a TODO added is counted" <|
            \_ ->
                let
                    labeled =
                        ProjectQuestionnaire.setLabels value1Path [ ProjectQuestionnaire.todoUuid ] (withValue1 Nothing)

                    event =
                        ProjectEvent.SetLabels { uuid = Uuid.nil, path = value1Path, value = [ ProjectQuestionnaire.todoUuid ], createdAt = Time.millisToPosix 0, createdBy = Nothing }
                in
                Rebuild.updateTodos event labeled []
                    |> List.map .path
                    |> Expect.equal [ value1Path ]
        , test "typing keeps the TODOs as they were" <|
            \_ ->
                Rebuild.updateTodos (setReply value1Path (StringReply "a")) (withValue1 (Just "a")) []
                    |> Expect.equal []
        , test "typing that brings no warning keeps the warnings" <|
            \_ ->
                Rebuild.updateWarnings (setReply value1Path (StringReply "ab")) (withValue1 (Just "ab")) []
                    |> Expect.equal []
        , test "a clear works the warnings out again" <|
            \_ ->
                Rebuild.updateWarnings (clearReply value1Path) (withValue1 Nothing) (ProjectQuestionnaire.getWarnings (withValue1 Nothing))
                    |> Expect.equal []
        ]
