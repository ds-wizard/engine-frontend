module Common.Components.Questionnaire.FlagsTest exposing (flagsTest)

import Common.Components.Questionnaire.Flags as Flags
import Common.Components.Questionnaire.ViewSettings as ViewSettings
import Expect
import Test exposing (Test, describe, test)


flagsTest : Test
flagsTest =
    let
        everything =
            Flags.base { commentsEnabled = True, readOnly = True, showActions = True } ViewSettings.all

        nothing =
            Flags.base { commentsEnabled = False, readOnly = False, showActions = False } ViewSettings.none

        pageWide flags =
            [ Flags.commentsEnabled flags
            , Flags.isReadOnly flags
            , Flags.showActions flags
            , Flags.showAnsweredBy flags
            , Flags.showMetricValues flags
            , Flags.showPhases flags
            , Flags.showTags flags
            ]

        perNode flags =
            [ Flags.hasCommentsOpen flags
            , Flags.hasTodo flags
            , Flags.isHighlighted flags
            , Flags.hasCopiedLink flags
            ]

        allPerNode =
            Flags.withCommentsOpen True
                >> Flags.withTodo True
                >> Flags.withHighlight True
                >> Flags.withCopiedLink True
    in
    describe "Flags"
        [ test "carries every page-wide switch" <|
            \_ -> pageWide everything |> Expect.equal (List.repeat 7 True)
        , test "carries none of them when they are off" <|
            \_ -> pageWide nothing |> Expect.equal (List.repeat 7 False)
        , test "adds each per-node bit on its own" <|
            \_ -> perNode (allPerNode nothing) |> Expect.equal (List.repeat 4 True)
        , test "keeps the page-wide switches when a node adds its bits" <|
            \_ -> pageWide (allPerNode everything) |> Expect.equal (List.repeat 7 True)
        , test "clears a bit that is set to false" <|
            \_ ->
                Flags.withCopiedLink True nothing
                    |> Flags.withCopiedLink False
                    |> Expect.equal nothing
        , test "gives two nodes that differ in one state two different numbers, and two alike the same" <|
            \_ ->
                ( Flags.withTodo True everything == everything
                , Flags.withTodo False everything == everything
                )
                    |> Expect.equal ( False, True )
        ]
