module Common.Pages.DocumentTemplateEditors.Editor.GroupsTest exposing (suite)

{-| What is open in the document template editor, and on which side.

The rules worth pinning down are the ones that keep the two sides honest: a
file is not opened twice, closing the last tab of a side takes the side away,
and moving the one open file to the other side and back leaves what there was.

-}

import Common.Pages.DocumentTemplateEditors.Editor.Files as Files
import Common.Pages.DocumentTemplateEditors.Editor.Groups as Groups exposing (Side(..))
import Expect
import Set
import Test exposing (Test, describe, test)


a : Files.Entry
a =
    Files.File "a"


b : Files.Entry
b =
    Files.File "b"


c : Files.Entry
c =
    Files.Asset "c"


suite : Test
suite =
    describe "Common.Pages.DocumentTemplateEditors.Editor.Groups"
        [ test "opening a file adds a tab and shows it" <|
            \_ ->
                Groups.init
                    |> Groups.open a
                    |> Groups.open b
                    |> (\groups -> ( Groups.tabsOf First groups, Groups.currentOf First groups ))
                    |> Expect.equal ( [ a, b ], Just b )
        , test "opening what is already open brings it forward rather than opening it twice" <|
            \_ ->
                Groups.init
                    |> Groups.open a
                    |> Groups.open b
                    |> Groups.open a
                    |> (\groups -> ( Groups.tabsOf First groups, Groups.currentOf First groups ))
                    |> Expect.equal ( [ a, b ], Just a )
        , test "moving the one open file to the other side splits and moves the work there" <|
            \_ ->
                let
                    groups =
                        Groups.init
                            |> Groups.open a
                            |> Groups.open b
                            |> Groups.moveToOtherSide First
                in
                ( Groups.tabsOf First groups, Groups.tabsOf Second groups, Groups.activeSide groups )
                    |> Expect.equal ( [ a ], [ b ], Second )
        , test "moving the only open file to the other side leaves it where it is" <|
            \_ ->
                Groups.init
                    |> Groups.open a
                    |> Groups.moveToOtherSide First
                    |> (\groups -> ( Groups.isSplit groups, Groups.tabsOf First groups ))
                    |> Expect.equal ( False, [ a ] )
        , test "and the one on the other side comes back when it is sent back" <|
            \_ ->
                Groups.init
                    |> Groups.open a
                    |> Groups.open b
                    |> Groups.moveToOtherSide First
                    |> Groups.moveToOtherSide Second
                    |> (\groups -> ( Groups.isSplit groups, Groups.tabsOf First groups ))
                    |> Expect.equal ( False, [ a, b ] )
        , test "the last file of one side joins the other rather than swapping the two" <|
            \_ ->
                let
                    groups =
                        Groups.init
                            |> Groups.open a
                            |> Groups.open b
                            -- b to the second side, a left alone on the first.
                            |> Groups.moveToOtherSide First
                            |> Groups.moveToOtherSide First
                in
                ( Groups.isSplit groups, Groups.tabsOf First groups, Groups.currentOf First groups )
                    |> Expect.equal ( False, [ b, a ], Just a )
        , test "a file moved to the other side is not open twice" <|
            \_ ->
                Groups.init
                    |> Groups.open a
                    |> Groups.open b
                    |> Groups.moveToOtherSide First
                    |> Groups.openEntries
                    |> Expect.equalLists [ a, b ]
        , test "closing the shown tab shows what is left" <|
            \_ ->
                Groups.init
                    |> Groups.open a
                    |> Groups.open b
                    |> Groups.close First b
                    |> Groups.currentOf First
                    |> Expect.equal (Just a)
        , test "emptying the first side gives it to the second" <|
            \_ ->
                let
                    groups =
                        Groups.init
                            |> Groups.open a
                            |> Groups.open b
                            |> Groups.moveToOtherSide First
                            |> Groups.close First a
                in
                ( Groups.isSplit groups, Groups.tabsOf First groups, Groups.currentOf First groups )
                    |> Expect.equal ( False, [ b ], Just b )
        , test "a deleted file loses its tab, on whichever side it is, and what is left keeps its side" <|
            \_ ->
                let
                    groups =
                        Groups.init
                            |> Groups.open a
                            |> Groups.open b
                            |> Groups.moveToOtherSide First
                            -- c opens beside b, on the side the work moved to.
                            |> Groups.open c
                            |> Groups.keepOnly (Set.fromList [ "a", "c" ])
                in
                ( Groups.openEntries groups, Groups.tabsOf First groups, Groups.tabsOf Second groups )
                    |> Expect.equal ( [ a, c ], [ a ], [ c ] )
        , test "opening a file already open on the other side works there rather than here" <|
            \_ ->
                let
                    groups =
                        Groups.init
                            |> Groups.open a
                            |> Groups.open b
                            |> Groups.moveToOtherSide First
                            |> Groups.open a
                in
                ( Groups.activeSide groups, Groups.tabsOf First groups, Groups.tabsOf Second groups )
                    |> Expect.equal ( First, [ a ], [ b ] )
        , test "activating names both the tab and the side being worked on" <|
            \_ ->
                Groups.init
                    |> Groups.open a
                    |> Groups.open b
                    |> Groups.moveToOtherSide First
                    |> Groups.activate First a
                    |> (\groups -> ( Groups.activeSide groups, Groups.currentOf First groups ))
                    |> Expect.equal ( First, Just a )
        ]
