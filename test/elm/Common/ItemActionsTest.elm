module Common.ItemActionsTest exposing (groupsTests, testIdTests)

import Common.Components.ItemActions as ItemActions exposing (Action)
import Expect
import Test exposing (Test, describe, test)
import Ui.Icon as Icon


open : Action String
open =
    ItemActions.link { icon = Icon.solid "fa-folder-open", label = "Open", url = "/open" }


export : Action String
export =
    ItemActions.action { icon = Icon.solid "fa-download", label = "Export", onSelect = "export" }


delete : Action String
delete =
    ItemActions.dangerAction { icon = Icon.solid "fa-trash", label = "Delete", onSelect = "delete" }


{-| ( actions, of which separators ).
-}
shape : List (Action String) -> ( Int, Int )
shape actions =
    let
        stops =
            ItemActions.focusableCount actions
    in
    ( stops, List.length actions - stops )


groupsTests : Test
groupsTests =
    describe "groups"
        [ test "a separator between non-empty groups" <|
            \_ ->
                ItemActions.groups [ [ ( open, True ), ( export, True ) ], [ ( delete, True ) ] ]
                    |> shape
                    |> Expect.equal ( 3, 1 )
        , test "hidden actions are dropped" <|
            \_ ->
                ItemActions.groups [ [ ( open, True ), ( export, False ) ], [ ( delete, True ) ] ]
                    |> shape
                    |> Expect.equal ( 2, 1 )
        , test "a group left empty gets no separator" <|
            \_ ->
                ItemActions.groups [ [ ( open, True ) ], [ ( export, False ) ], [ ( delete, True ) ] ]
                    |> shape
                    |> Expect.equal ( 2, 1 )
        , test "no leading or trailing separator" <|
            \_ ->
                ItemActions.groups [ [ ( open, False ) ], [ ( export, True ) ], [ ( delete, False ) ] ]
                    |> shape
                    |> Expect.equal ( 1, 0 )
        , test "nothing visible is nothing" <|
            \_ ->
                ItemActions.groups [ [ ( open, False ) ], [] ]
                    |> List.length
                    |> Expect.equal 0
        ]


testIdTests : Test
testIdTests =
    describe "withTestId"
        [ test "keeps an action a stop for the arrow keys" <|
            \_ ->
                [ ItemActions.withTestId "open" open, ItemActions.withTestId "delete" delete ]
                    |> shape
                    |> Expect.equal ( 2, 0 )
        , test "survives groups" <|
            \_ ->
                ItemActions.groups [ [ ( ItemActions.withTestId "open" open, True ) ], [ ( ItemActions.withTestId "export" export, True ) ] ]
                    |> shape
                    |> Expect.equal ( 2, 1 )
        ]
