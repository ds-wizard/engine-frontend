module Wizard.Components.Questionnaire2.ToolbarViewFlags exposing (ToolbarViewFlags, commentsVisible, importersVisible, toInt, todosVisible, versionHistoryVisible)

import Bitwise


type alias ToolbarViewFlags =
    { todosVisible : Bool
    , commentsVisible : Bool
    , versionHistoryVisible : Bool
    , importersVisible : Bool
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


importersVisibleMask : Int
importersVisibleMask =
    8


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
        |> addFlag flags.importersVisible importersVisibleMask


todosVisible : Int -> Bool
todosVisible =
    checkFlag todosVisibleMask


commentsVisible : Int -> Bool
commentsVisible =
    checkFlag commentsVisibleMask


versionHistoryVisible : Int -> Bool
versionHistoryVisible =
    checkFlag versionHistoryVisibleMask


importersVisible : Int -> Bool
importersVisible =
    checkFlag importersVisibleMask


checkFlag : Int -> Int -> Bool
checkFlag mask flags =
    Bitwise.and flags mask /= 0
