module Common.Components.KMComparison.RowsTest exposing (suite)

import Common.Api.Models.KnowledgeModel exposing (KnowledgeModel)
import Common.Api.Models.KnowledgeModel.MetricMeasure exposing (MetricMeasure)
import Common.Components.KMComparison.Fixture as Fixture
import Common.Components.KMComparison.Rows as Rows exposing (Row, State(..))
import Expect
import Gettext
import Set exposing (Set)
import Test exposing (Test, describe, test)


{-| What a row is, for a test: how deep it is, what happened to it, and what it
draws in each column. A `Nothing` column is a column the row does not fill.
-}
type alias Summary =
    ( Int, State, ( Maybe String, Maybe String ) )


fold : KnowledgeModel -> KnowledgeModel -> List Summary
fold left right =
    foldCollapsed Set.empty left right


foldCollapsed : Set String -> KnowledgeModel -> KnowledgeModel -> List Summary
foldCollapsed collapsed left right =
    Rows.fold
        { locale = Gettext.defaultLocale
        , leftKm = left
        , rightKm = right
        , collapsed = collapsed
        }
        |> List.map summarize


summarize : Row -> Summary
summarize row =
    ( row.level, row.state, ( row.left, row.right ) )


carets : KnowledgeModel -> KnowledgeModel -> List ( Maybe String, Bool )
carets left right =
    Rows.fold
        { locale = Gettext.defaultLocale
        , leftKm = left
        , rightKm = right
        , collapsed = Set.empty
        }
        |> List.map (\row -> ( row.left, row.hasChildren ))



-- KNOWLEDGE MODELS


oneChapter : String -> KnowledgeModel
oneChapter title =
    Fixture.km
        |> Fixture.withChapters [ Fixture.chapter "ch1" title ]
        |> Fixture.withQuestions "ch1" [ Fixture.valueQuestion "q1" "Question 1" ]


chapterWithAnswers : String -> List MetricMeasure -> KnowledgeModel
chapterWithAnswers label measures =
    Fixture.km
        |> Fixture.withChapters [ Fixture.chapter "ch1" "Chapter" ]
        |> Fixture.withQuestions "ch1" [ Fixture.optionsQuestion "q1" "Question" ]
        |> Fixture.withAnswers "q1" [ Fixture.answer "a1" label measures ]


suite : Test
suite =
    describe "Common.Components.KMComparison.Rows"
        [ describe "sections"
            [ test "a knowledge model with nothing in it has no rows at all" <|
                \_ ->
                    fold Fixture.km Fixture.km |> Expect.equal []
            , test "a section is a row, and its items sit at its own level" <|
                \_ ->
                    fold (oneChapter "Chapter") (oneChapter "Chapter")
                        |> Expect.equal
                            [ ( 0, Section, ( Just "Chapters", Just "Chapters" ) )
                            , ( 0, Unchanged, ( Just "Chapter", Just "Chapter" ) )
                            , ( 1, Section, ( Just "Questions", Just "Questions" ) )
                            , ( 1, Unchanged, ( Just "Question 1", Just "Question 1" ) )
                            ]
            , test "an added parent's section draws only in the right column" <|
                \_ ->
                    fold Fixture.km (oneChapter "Chapter")
                        |> Expect.equal
                            [ ( 0, Section, ( Just "Chapters", Just "Chapters" ) )
                            , ( 0, Added, ( Nothing, Just "Chapter" ) )
                            , ( 1, Section, ( Nothing, Just "Questions" ) )
                            , ( 1, Added, ( Nothing, Just "Question 1" ) )
                            ]
            , test "a removed parent's section draws only in the left column" <|
                \_ ->
                    fold (oneChapter "Chapter") Fixture.km
                        |> Expect.equal
                            [ ( 0, Section, ( Just "Chapters", Just "Chapters" ) )
                            , ( 0, Removed, ( Just "Chapter", Nothing ) )
                            , ( 1, Section, ( Just "Questions", Nothing ) )
                            , ( 1, Removed, ( Just "Question 1", Nothing ) )
                            ]
            , test "a chapter with no questions emits no Questions section" <|
                \_ ->
                    let
                        bare =
                            Fixture.withChapters [ Fixture.chapter "ch1" "Chapter" ] Fixture.km
                    in
                    fold bare bare
                        |> Expect.equal
                            [ ( 0, Section, ( Just "Chapters", Just "Chapters" ) )
                            , ( 0, Unchanged, ( Just "Chapter", Just "Chapter" ) )
                            ]
            ]
        , describe "what changed"
            [ test "a chapter whose title changed is Changed, and its untouched questions are unchanged" <|
                \_ ->
                    fold (oneChapter "Before") (oneChapter "After")
                        |> Expect.equal
                            [ ( 0, Section, ( Just "Chapters", Just "Chapters" ) )
                            , ( 0, Changed, ( Just "Before", Just "After" ) )
                            , ( 1, Section, ( Just "Questions", Just "Questions" ) )
                            , ( 1, Unchanged, ( Just "Question 1", Just "Question 1" ) )
                            ]
            , test "a reordered chapter is removed and added, not unchanged" <|
                \_ ->
                    let
                        two first second =
                            Fixture.withChapters
                                [ Fixture.chapter first (String.toUpper first)
                                , Fixture.chapter second (String.toUpper second)
                                ]
                                Fixture.km
                    in
                    fold (two "ch1" "ch2") (two "ch2" "ch1")
                        |> Expect.equal
                            [ ( 0, Section, ( Just "Chapters", Just "Chapters" ) )
                            , ( 0, Removed, ( Just "CH1", Nothing ) )
                            , ( 0, Unchanged, ( Just "CH2", Just "CH2" ) )
                            , ( 0, Added, ( Nothing, Just "CH1" ) )
                            ]
            , test "one entity added and a different one removed produce two rows" <|
                \_ ->
                    let
                        metrics uuid title =
                            Fixture.withMetrics [ Fixture.metric uuid title ] Fixture.km
                    in
                    fold (metrics "m1" "Old") (metrics "m2" "New")
                        |> Expect.equal
                            [ ( 0, Section, ( Just "Metrics", Just "Metrics" ) )
                            , ( 0, Removed, ( Just "Old", Nothing ) )
                            , ( 0, Added, ( Nothing, Just "New" ) )
                            ]
            ]
        , describe "a subtree on one side only is not diffed"
            [ test "an added chapter's questions are all added" <|
                \_ ->
                    let
                        left =
                            Fixture.km
                                |> Fixture.withChapters [ Fixture.chapter "ch1" "Kept" ]

                        right =
                            Fixture.km
                                |> Fixture.withChapters [ Fixture.chapter "ch1" "Kept", Fixture.chapter "ch2" "New" ]
                                |> Fixture.withQuestions "ch2" [ Fixture.valueQuestion "q1" "Question 1" ]
                    in
                    fold left right
                        |> Expect.equal
                            [ ( 0, Section, ( Just "Chapters", Just "Chapters" ) )
                            , ( 0, Unchanged, ( Just "Kept", Just "Kept" ) )
                            , ( 0, Added, ( Nothing, Just "New" ) )
                            , ( 1, Section, ( Nothing, Just "Questions" ) )
                            , ( 1, Added, ( Nothing, Just "Question 1" ) )
                            ]
            , test "a removed answer takes its follow-up questions with it" <|
                \_ ->
                    let
                        withFollowUp =
                            chapterWithAnswers "Yes" []
                                |> Fixture.withFollowUps "a1" [ Fixture.valueQuestion "q2" "Follow-up" ]

                        without =
                            Fixture.km
                                |> Fixture.withChapters [ Fixture.chapter "ch1" "Chapter" ]
                                |> Fixture.withQuestions "ch1" [ Fixture.optionsQuestion "q1" "Question" ]
                    in
                    fold withFollowUp without
                        |> Expect.equal
                            [ ( 0, Section, ( Just "Chapters", Just "Chapters" ) )
                            , ( 0, Unchanged, ( Just "Chapter", Just "Chapter" ) )
                            , ( 1, Section, ( Just "Questions", Just "Questions" ) )
                            , ( 1, Unchanged, ( Just "Question", Just "Question" ) )
                            , ( 2, Section, ( Just "Answers", Just "Answers" ) )
                            , ( 2, Removed, ( Just "Yes", Nothing ) )
                            , ( 3, Section, ( Just "Follow-up Questions", Nothing ) )
                            , ( 3, Removed, ( Just "Follow-up", Nothing ) )
                            ]
            ]
        , describe "metric measures are content, not children"
            [ test "an answer whose measures changed is changed, and no metric row appears under it" <|
                \_ ->
                    fold (chapterWithAnswers "Yes" [ Fixture.measure "m1" 0.5 ])
                        (chapterWithAnswers "Yes" [ Fixture.measure "m1" 1 ])
                        |> Expect.equal
                            [ ( 0, Section, ( Just "Chapters", Just "Chapters" ) )
                            , ( 0, Unchanged, ( Just "Chapter", Just "Chapter" ) )
                            , ( 1, Section, ( Just "Questions", Just "Questions" ) )
                            , ( 1, Unchanged, ( Just "Question", Just "Question" ) )
                            , ( 2, Section, ( Just "Answers", Just "Answers" ) )
                            , ( 2, Changed, ( Just "Yes", Just "Yes" ) )
                            ]
            ]
        , describe "what a row hands the panel"
            [ test "an entity row's id is the uuid of the entity it carries" <|
                \_ ->
                    let
                        left =
                            chapterWithAnswers "Yes" []
                                |> Fixture.withMetrics [ Fixture.metric "m1" "Metric" ]

                        right =
                            chapterWithAnswers "No" []
                                |> Fixture.withMetrics [ Fixture.metric "m1" "Metric" ]
                    in
                    Rows.fold
                        { locale = Gettext.defaultLocale
                        , leftKm = left
                        , rightKm = right
                        , collapsed = Set.empty
                        }
                        |> List.filterMap (\row -> Maybe.map (\entity -> ( row.id, Rows.entityUuid entity )) row.entity)
                        |> Expect.equalLists
                            [ ( "ch1", "ch1" ), ( "q1", "q1" ), ( "a1", "a1" ), ( "m1", "m1" ) ]
            , test "a section row hands the panel nothing" <|
                \_ ->
                    Rows.fold
                        { locale = Gettext.defaultLocale
                        , leftKm = oneChapter "Chapter"
                        , rightKm = oneChapter "Chapter"
                        , collapsed = Set.empty
                        }
                        |> List.filter (\row -> row.state == Section)
                        |> List.map .entity
                        |> Expect.equal [ Nothing, Nothing ]
            ]
        , describe "collapsing"
            [ test "a collapsed chapter drops its questions and its Questions section" <|
                \_ ->
                    foldCollapsed (Set.singleton "ch1") (oneChapter "Chapter") (oneChapter "Chapter")
                        |> Expect.equal
                            [ ( 0, Section, ( Just "Chapters", Just "Chapters" ) )
                            , ( 0, Unchanged, ( Just "Chapter", Just "Chapter" ) )
                            ]
            , test "a collapsed chapter keeps its caret" <|
                \_ ->
                    Rows.fold
                        { locale = Gettext.defaultLocale
                        , leftKm = oneChapter "Chapter"
                        , rightKm = oneChapter "Chapter"
                        , collapsed = Set.singleton "ch1"
                        }
                        |> List.filter (\row -> row.state /= Section)
                        |> List.map .hasChildren
                        |> Expect.equal [ True ]
            , test "a chapter with no questions has no caret" <|
                \_ ->
                    let
                        bare =
                            Fixture.withChapters [ Fixture.chapter "ch1" "Chapter" ] Fixture.km
                    in
                    carets bare bare
                        |> Expect.equal [ ( Just "Chapters", False ), ( Just "Chapter", False ) ]
            ]
        ]
