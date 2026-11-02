module Common.Components.Questionnaire.Flags exposing
    ( Flags
    , base
    , commentsEnabled
    , hasCommentsOpen
    , hasCopiedLink
    , hasTodo
    , isHighlighted
    , isReadOnly
    , showActions
    , showAnsweredBy
    , showMetricValues
    , showPhases
    , showTags
    , withCommentsOpen
    , withCopiedLink
    , withHighlight
    , withTodo
    )

{-| Everything a question node needs to know that is a yes or a no, as one `Int`.

`Html.Lazy` compares its arguments by reference, and an `Int` by value: a record
of booleans built on every render would never be equal to the last one, and each
boolean passed on its own would use up the eight arguments lazy allows. So the
page-wide switches are packed once per render (`base`), and each node adds its
own bits to that number with plain integer arithmetic — whether its comments are
open, whether it has a TODO, whether it is highlighted, whether its link was just
copied.

**A per-node state is a per-node bit.** A state that is true of one node must
change only that node's number, or every node re-renders when it changes. The
wizard folded "a link was just copied" into the page-wide switches, so copying one
link re-rendered every question; here it is `withCopiedLink`, set for the one
node whose link it was. (An open plugin panel is told to its node the same way,
through `ContentMsg.QuestionActions` rather than a bit, because the node needs to
know which of its actions is open.)

-}

import Bitwise
import Common.Components.Questionnaire.ViewSettings exposing (ViewSettings)


type alias Flags =
    Int


{-| The switches that are the same for every node on the page.
-}
base : { commentsEnabled : Bool, readOnly : Bool, showActions : Bool } -> ViewSettings -> Flags
base config settings =
    0
        |> set config.commentsEnabled commentsEnabledMask
        |> set config.readOnly readOnlyMask
        |> set config.showActions showActionsMask
        |> set settings.answeredBy showAnsweredByMask
        |> set settings.metricValues showMetricValuesMask
        |> set settings.phases showPhasesMask
        |> set settings.tags showTagsMask


withCommentsOpen : Bool -> Flags -> Flags
withCommentsOpen value =
    set value commentsOpenMask


withTodo : Bool -> Flags -> Flags
withTodo value =
    set value todoMask


withHighlight : Bool -> Flags -> Flags
withHighlight value =
    set value highlightMask


withCopiedLink : Bool -> Flags -> Flags
withCopiedLink value =
    set value copiedLinkMask


commentsEnabled : Flags -> Bool
commentsEnabled =
    has commentsEnabledMask


isReadOnly : Flags -> Bool
isReadOnly =
    has readOnlyMask


showActions : Flags -> Bool
showActions =
    has showActionsMask


showAnsweredBy : Flags -> Bool
showAnsweredBy =
    has showAnsweredByMask


showMetricValues : Flags -> Bool
showMetricValues =
    has showMetricValuesMask


showPhases : Flags -> Bool
showPhases =
    has showPhasesMask


showTags : Flags -> Bool
showTags =
    has showTagsMask


hasCommentsOpen : Flags -> Bool
hasCommentsOpen =
    has commentsOpenMask


hasTodo : Flags -> Bool
hasTodo =
    has todoMask


isHighlighted : Flags -> Bool
isHighlighted =
    has highlightMask


hasCopiedLink : Flags -> Bool
hasCopiedLink =
    has copiedLinkMask



-- BITS


commentsEnabledMask : Int
commentsEnabledMask =
    1


readOnlyMask : Int
readOnlyMask =
    2


showActionsMask : Int
showActionsMask =
    4


showAnsweredByMask : Int
showAnsweredByMask =
    8


showMetricValuesMask : Int
showMetricValuesMask =
    16


showPhasesMask : Int
showPhasesMask =
    32


showTagsMask : Int
showTagsMask =
    64


commentsOpenMask : Int
commentsOpenMask =
    128


todoMask : Int
todoMask =
    256


highlightMask : Int
highlightMask =
    512


copiedLinkMask : Int
copiedLinkMask =
    1024


set : Bool -> Int -> Flags -> Flags
set value mask flags =
    if value then
        Bitwise.or flags mask

    else
        Bitwise.and flags (Bitwise.complement mask)


has : Int -> Flags -> Bool
has mask flags =
    Bitwise.and flags mask /= 0
