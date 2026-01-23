module Wizard.Components.Questionnaire2.QuestionViewFlags exposing
    ( QuestionViewFlags
    , addHasCommentsOpen
    , addHasTodo
    , addIsHighlighted
    , commentsEnabled
    , fromQuestionnaireViewSettings
    , hasCommentsOpen
    , hasTodo
    , isHighlighted
    , isReadOnly
    , recentlyCopiedLink
    , showActions
    , showAnsweredBy
    , showMetricValues
    , showPhases
    , showTags
    , toInt
    )

import Bitwise
import Wizard.Components.Questionnaire2.QuestionnaireViewSettings exposing (QuestionnaireViewSettings)


type alias QuestionViewFlags =
    { commentsEnabled : Bool
    , hasCommentsOpen : Bool
    , hasTodo : Bool
    , isHighlighted : Bool
    , isReadOnly : Bool
    , recentlyCopiedLink : Bool
    , showActions : Bool
    , showAnsweredBy : Bool
    , showMetricValues : Bool
    , showPhases : Bool
    , showTags : Bool
    }


fromQuestionnaireViewSettings : QuestionnaireViewSettings -> Bool -> Bool -> Bool -> Bool -> QuestionViewFlags
fromQuestionnaireViewSettings settings commentsEnabledValue showActionsValue isReadOnlyValue recentlyCopiedValue =
    { commentsEnabled = commentsEnabledValue
    , hasCommentsOpen = False
    , hasTodo = False
    , isHighlighted = False
    , isReadOnly = isReadOnlyValue
    , recentlyCopiedLink = recentlyCopiedValue
    , showActions = showActionsValue
    , showAnsweredBy = settings.answeredBy
    , showMetricValues = settings.metricValues
    , showPhases = settings.phases
    , showTags = settings.tags
    }


addHasCommentsOpen : Bool -> Int -> Int
addHasCommentsOpen newHasCommentsOpen flags =
    if newHasCommentsOpen then
        Bitwise.or flags hasCommentsOpenMask

    else
        flags


addHasTodo : Bool -> Int -> Int
addHasTodo newHasTodo flags =
    if newHasTodo then
        Bitwise.or flags hasTodoMask

    else
        flags


addIsHighlighted : Bool -> Int -> Int
addIsHighlighted newIsHighlighted flags =
    if newIsHighlighted then
        Bitwise.or flags isHighlightedMask

    else
        flags


commentsEnabledMask : Int
commentsEnabledMask =
    1024


hasCommentsOpenMask : Int
hasCommentsOpenMask =
    1


hasTodoMask : Int
hasTodoMask =
    2


isHighlightedMask : Int
isHighlightedMask =
    4


isReadOnlyMask : Int
isReadOnlyMask =
    8


recentlyCopiedLinkMask : Int
recentlyCopiedLinkMask =
    16


showActionsMask : Int
showActionsMask =
    32


showAnsweredByMask : Int
showAnsweredByMask =
    64


showMetricValuesMask : Int
showMetricValuesMask =
    128


showPhasesMask : Int
showPhasesMask =
    256


showTagsMask : Int
showTagsMask =
    512


toInt : QuestionViewFlags -> Int
toInt flags =
    let
        addFlag flag value acc =
            if flag then
                acc + value

            else
                acc
    in
    0
        |> addFlag flags.commentsEnabled commentsEnabledMask
        |> addFlag flags.hasCommentsOpen hasCommentsOpenMask
        |> addFlag flags.hasTodo hasTodoMask
        |> addFlag flags.isHighlighted isHighlightedMask
        |> addFlag flags.isReadOnly isReadOnlyMask
        |> addFlag flags.recentlyCopiedLink recentlyCopiedLinkMask
        |> addFlag flags.showActions showActionsMask
        |> addFlag flags.showAnsweredBy showAnsweredByMask
        |> addFlag flags.showMetricValues showMetricValuesMask
        |> addFlag flags.showPhases showPhasesMask
        |> addFlag flags.showTags showTagsMask


commentsEnabled : Int -> Bool
commentsEnabled =
    checkFlag commentsEnabledMask


hasCommentsOpen : Int -> Bool
hasCommentsOpen =
    checkFlag hasCommentsOpenMask


hasTodo : Int -> Bool
hasTodo =
    checkFlag hasTodoMask


isHighlighted : Int -> Bool
isHighlighted =
    checkFlag isHighlightedMask


isReadOnly : Int -> Bool
isReadOnly =
    checkFlag isReadOnlyMask


recentlyCopiedLink : Int -> Bool
recentlyCopiedLink =
    checkFlag recentlyCopiedLinkMask


showActions : Int -> Bool
showActions =
    checkFlag showActionsMask


showAnsweredBy : Int -> Bool
showAnsweredBy =
    checkFlag showAnsweredByMask


showMetricValues : Int -> Bool
showMetricValues =
    checkFlag showMetricValuesMask


showPhases : Int -> Bool
showPhases =
    checkFlag showPhasesMask


showTags : Int -> Bool
showTags =
    checkFlag showTagsMask


checkFlag : Int -> Int -> Bool
checkFlag mask flags =
    Bitwise.and flags mask /= 0
