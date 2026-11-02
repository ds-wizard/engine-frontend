module Ui.ReorderableListTest exposing (suite)

import Expect
import Html
import Html.Attributes as Attr
import Json.Encode
import Test exposing (Test, describe, test)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, class, tag, text)
import Ui.ReorderableList as ReorderableList


type Msg
    = Moved Int Int
    | Added
    | Dragged (Maybe ReorderableList.Dragging)


{-| What a message carries, so a test can compare the move and the drag rather
than only that one happened.
-}
moveOf : Msg -> Maybe ( Int, Int )
moveOf msg =
    case msg of
        Moved from to ->
            Just ( from, to )

        _ ->
            Nothing


dragOf : Msg -> Maybe (Maybe ReorderableList.Dragging)
dragOf msg =
    case msg of
        Dragged state ->
            Just state

        _ ->
            Nothing


dragging : Int -> Int -> ReorderableList.Dragging
dragging from over =
    { list = "chapters", from = from, over = over }


rows : List (ReorderableList.Item Msg)
rows =
    List.map
        (\name -> ReorderableList.item { key = name, label = name, content = [ Html.text name ] })
        [ "Administrative", "Data", "Sharing" ]


list : Maybe ReorderableList.Dragging -> Query.Single Msg
list drag =
    Query.fromHtml
        (ReorderableList.toHtml
            { id = "chapters"
            , label = "Chapters"
            , items = rows
            , onMove = Moved
            , announcement = Just "Data is now 2 of 3"
            , add = Just { label = "Add chapter", onAdd = Added }
            , drag = Just { state = drag, onChange = Dragged }
            }
        )


{-| What the list promises: a keyboard can do everything a pointer can, the
order is announced when it changes, and a drag is an addition that the buttons
never depend on.
-}
suite : Test
suite =
    describe "Ui.ReorderableList"
        [ test "the rows are a named list" <|
            \_ ->
                list Nothing
                    |> Query.find [ tag "ol" ]
                    |> Query.has [ attribute (Attr.attribute "aria-label" "Chapters") ]
        , test "the move is announced politely" <|
            \_ ->
                list Nothing
                    |> Query.find [ attribute (Attr.attribute "aria-live" "polite") ]
                    |> Query.has [ text "Data is now 2 of 3" ]
        , test "the add button is the list's own, under the rows" <|
            \_ ->
                list Nothing
                    |> Query.find [ class "ui-reorderable__add" ]
                    |> Query.find [ tag "button" ]
                    |> Event.simulate Event.click
                    |> Event.expect Added
        , test "the handle starts a drag on the row it is in" <|
            \_ ->
                list Nothing
                    |> Query.findAll [ class "ui-reorderable__grip" ]
                    |> Query.index 1
                    |> Event.simulate (Event.custom "dragstart" (Json.Encode.object []))
                    |> Event.toResult
                    |> Result.map dragOf
                    |> Expect.equal (Ok (Just (Just { list = "chapters", from = 1, over = 1 })))
        , test "the handle is hidden from assistive technology — the buttons say what it does" <|
            \_ ->
                list Nothing
                    |> Query.findAll [ class "ui-reorderable__grip" ]
                    |> Query.index 0
                    |> Query.has [ attribute (Attr.attribute "aria-hidden" "true") ]
        , test "dragging over a row makes it the target" <|
            \_ ->
                list (Just (dragging 0 0))
                    |> Query.findAll [ class "ui-reorderable__item" ]
                    |> Query.index 2
                    |> Event.simulate (Event.custom "dragover" (Json.Encode.object []))
                    |> Event.toResult
                    |> Result.map dragOf
                    |> Expect.equal (Ok (Just (Just (dragging 0 2))))
        , test "dropping on a row moves the dragged row to it" <|
            \_ ->
                list (Just (dragging 0 2))
                    |> Query.findAll [ class "ui-reorderable__item" ]
                    |> Query.index 2
                    |> Event.simulate (Event.custom "drop" (Json.Encode.object []))
                    |> Event.toResult
                    |> Result.map moveOf
                    |> Expect.equal (Ok (Just ( 0, 2 )))
        , test "the row being dragged fades, and where it would land opens as a slot" <|
            \_ ->
                list (Just (dragging 0 2))
                    |> Expect.all
                        [ Query.findAll [ class "is-dragging" ] >> Query.count (Expect.equal 1)
                        , Query.findAll [ class "ui-reorderable__slot" ] >> Query.count (Expect.equal 1)
                        ]
        , test "releasing on the slot is what performs the move" <|
            \_ ->
                list (Just (dragging 0 2))
                    |> Query.find [ class "ui-reorderable__slot" ]
                    |> Event.simulate (Event.custom "drop" (Json.Encode.object []))
                    |> Event.toResult
                    |> Result.map moveOf
                    |> Expect.equal (Ok (Just ( 0, 2 )))
        , test "dragging over the slot holds the drag where it is rather than moving anything" <|
            \_ ->
                list (Just (dragging 0 2))
                    |> Query.find [ class "ui-reorderable__slot" ]
                    |> Event.simulate (Event.custom "dragover" (Json.Encode.object []))
                    |> Event.toResult
                    |> Result.map dragOf
                    |> Expect.equal (Ok (Just (Just (dragging 0 2))))
        , test "no slot while the pointer is over the row's own place — there is no move to show" <|
            \_ ->
                list (Just (dragging 1 1))
                    |> Query.hasNot [ class "ui-reorderable__slot" ]
        , test "a drag in another list leaves this one alone" <|
            \_ ->
                list (Just { list = "experts", from = 0, over = 2 })
                    |> Expect.all
                        [ Query.hasNot [ class "ui-reorderable__slot" ]
                        , Query.hasNot [ class "is-dragging" ]
                        ]
        , test "without a drag there is no handle at all" <|
            \_ ->
                Query.fromHtml
                    (ReorderableList.toHtml
                        { id = "chapters"
                        , label = "Chapters"
                        , items = rows
                        , onMove = Moved
                        , announcement = Nothing
                        , add = Nothing
                        , drag = Nothing
                        }
                    )
                    |> Query.hasNot [ class "ui-reorderable__grip" ]
        ]
