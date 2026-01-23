module Wizard.Components.Questionnaire2.ToolbarViewFlags exposing (ToolbarViewFlags, commentsVisible, toInt, todosVisible, versionHistoryVisible)

import Bitwise


type alias ToolbarViewFlags =
    { todosVisible : Bool
    , commentsVisible : Bool
    , versionHistoryVisible : Bool
    }


todosVisibleMask : Int
todosVisibleMask =
    1


commentsVisibleMask : Int
commentsVisibleMask =
    2


versionHistoryVisibleMask : Int
versionHistoryVisibleMask =
    4


toInt : ToolbarViewFlags -> Int
toInt flags =
    let
        addFlag flag value acc =
            if flag then
                acc + value

            else
                acc
    in
    0
        |> addFlag flags.todosVisible todosVisibleMask
        |> addFlag flags.commentsVisible commentsVisibleMask
        |> addFlag flags.versionHistoryVisible versionHistoryVisibleMask


todosVisible : Int -> Bool
todosVisible =
    checkFlag todosVisibleMask


commentsVisible : Int -> Bool
commentsVisible =
    checkFlag commentsVisibleMask


versionHistoryVisible : Int -> Bool
versionHistoryVisible =
    checkFlag versionHistoryVisibleMask


checkFlag : Int -> Int -> Bool
checkFlag mask flags =
    Bitwise.and flags mask /= 0
