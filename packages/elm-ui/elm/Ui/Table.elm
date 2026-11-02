module Ui.Table exposing
    ( Column
    , Sort
    , SortDirection(..)
    , Table
    , Words
    , actions
    , column
    , sortableColumn
    , table
    , toHtml
    , withCaption
    , withHeader
    , withRowTestId
    , withSort
    , withWords
    )

{-| The table owns header scope, its caption and the sort announcement, because
those are the parts that get dropped when a caller assembles a table by hand.

A sorted column carries `aria-sort`, and the change is announced through a live
region rather than left silent — a sighted user sees the arrow move, and this is
the equivalent.

-}

import Html exposing (Html, caption, div, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (attribute, class, classList, scope)
import Html.Events exposing (onClick)
import List.Extra as List
import Ui.Button as Button exposing (Button)
import Ui.TestId as TestId


type alias Column a msg =
    { label : String
    , render : a -> Html msg
    , sortKey : Maybe String
    , isActions : Bool
    , header : Maybe (Html msg)
    }


type SortDirection
    = Ascending
    | Descending


type alias Sort msg =
    { key : String
    , direction : SortDirection
    , onSort : String -> msg
    }


type Table a msg
    = Table
        { columns : List (Column a msg)
        , rows : List a
        , caption : Maybe String
        , sort : Maybe (Sort msg)
        , words : Words
        , rowTestId : Maybe (a -> String)
        }


{-| The sort announcement: `sorted` is given the label of the sorted column and
whether the order is ascending — "Sorted by name, ascending". English unless
`withWords` says otherwise.
-}
type alias Words =
    { sorted : String -> Bool -> String
    }


column : String -> (a -> Html msg) -> Column a msg
column label render =
    { label = label, render = render, sortKey = Nothing, isActions = False, header = Nothing }


sortableColumn : String -> String -> (a -> Html msg) -> Column a msg
sortableColumn key label render =
    { label = label, render = render, sortKey = Just key, isActions = False, header = Nothing }


{-| What can be done to a row: the last column, aligned to the end of the row,
and nothing but buttons.

Icon-only buttons (`Button.iconOnly`, `Button.dangerIconOnly` for destructive
ones, `Button.iconOnlyLink`), each shown with its label in a tooltip on hover and
on keyboard focus, so a row stays one line however many actions it has and what
an icon means is never left to the icon. They are drawn small, in the order
given; destructive ones read as destructive by color as well as by icon and
name. A row with nothing to do gets an empty cell, so the columns still line up.

Every row repeats the same labels, so give each button a name that says which
row it is for — `Button.withAccessibleName "Delete API key Laptop"` — beginning
with the label its tooltip shows, or a list of the page's buttons is a column of
identical "Delete"s.

-}
actions : String -> (a -> List (Button msg)) -> Column a msg
actions label render =
    { label = label
    , render = \row -> div [ class "ui-table__actions" ] (List.map (Button.toHtml << Button.withTooltip << Button.small) (render row))
    , sortKey = Nothing
    , isActions = True
    , header = Nothing
    }


{-| A header cell that holds a control rather than the label: the column's name
as a field to edit, in a table whose columns the reader makes — or, over the
actions column, the button that adds a column. The cell is still the column's
`th scope="col"`; the label stays the column's name for everything that is not
drawn, such as the sort announcement, so a control here says in its own
accessible name what it is for. A column with a header of its own is not
sortable.
-}
withHeader : Html msg -> Column a msg -> Column a msg
withHeader content col =
    { col | header = Just content }


table : List (Column a msg) -> List a -> Table a msg
table columns rows =
    Table
        { columns = columns
        , rows = rows
        , caption = Nothing
        , sort = Nothing
        , rowTestId = Nothing
        , words =
            { sorted =
                \key ascending ->
                    "Sorted by "
                        ++ key
                        ++ ", "
                        ++ (if ascending then
                                "ascending"

                            else
                                "descending"
                           )
            }
        }


withCaption : String -> Table a msg -> Table a msg
withCaption text_ (Table config) =
    Table { config | caption = Just text_ }


{-| The test hook on each row (`data-cy`), made from the row's data — its
uuid, say — so a test can find the row for one item without matching its text.
-}
withRowTestId : (a -> String) -> Table a msg -> Table a msg
withRowTestId toTestId (Table config) =
    Table { config | rowTestId = Just toTestId }


withSort : Sort msg -> Table a msg -> Table a msg
withSort sort (Table config) =
    Table { config | sort = Just sort }


{-| The table's own words in the language of the page.
-}
withWords : Words -> Table a msg -> Table a msg
withWords words (Table config) =
    Table { config | words = words }


toHtml : Table a msg -> Html msg
toHtml (Table config) =
    div [ class "ui-table-wrapper" ]
        [ Html.table [ class "ui-table" ]
            [ case config.caption of
                Just text_ ->
                    caption [ class "ui-table__caption" ] [ text text_ ]

                Nothing ->
                    text ""
            , thead [] [ tr [] (List.map (viewHeader config.sort) config.columns) ]
            , tbody [] (List.map (viewRow config.rowTestId config.columns) config.rows)
            ]
        , announcement config.words config.columns config.sort
        ]


viewHeader : Maybe (Sort msg) -> Column a msg -> Html msg
viewHeader sort col =
    let
        activeDirection =
            Maybe.andThen
                (\s ->
                    if Just s.key == col.sortKey then
                        Just s.direction

                    else
                        Nothing
                )
                sort

        sortAttr =
            case activeDirection of
                Just Ascending ->
                    [ attribute "aria-sort" "ascending" ]

                Just Descending ->
                    [ attribute "aria-sort" "descending" ]

                Nothing ->
                    []
    in
    case ( col.header, col.sortKey, sort ) of
        ( Just content, _, _ ) ->
            th [ scope "col", headerClass col ] [ content ]

        ( Nothing, Just key, Just s ) ->
            th (scope "col" :: headerClass col :: sortAttr)
                [ Html.button
                    [ class "ui-table__sort", onClick (s.onSort key) ]
                    [ text col.label ]
                ]

        _ ->
            th (scope "col" :: headerClass col :: sortAttr) [ text col.label ]


headerClass : Column a msg -> Html.Attribute msg
headerClass col =
    classList [ ( "ui-table__th", True ), ( "ui-table__th--actions", col.isActions ) ]


viewRow : Maybe (a -> String) -> List (Column a msg) -> a -> Html msg
viewRow rowTestId columns row =
    tr
        (class "ui-table__row"
            :: (case rowTestId of
                    Just toTestId ->
                        [ TestId.attribute (toTestId row) ]

                    Nothing ->
                        []
               )
        )
        (List.map
            (\col ->
                td [ classList [ ( "ui-table__td", True ), ( "ui-table__td--actions", col.isActions ) ] ]
                    [ col.render row ]
            )
            columns
        )


{-| A sighted user sees the arrow move; this is the equivalent for everyone else.
-}
announcement : Words -> List (Column a msg) -> Maybe (Sort msg) -> Html msg
announcement words columns sort =
    case sort of
        Just s ->
            div
                [ class "ui-visually-hidden"
                , attribute "aria-live" "polite"
                ]
                [ text (words.sorted (sortedLabel columns s.key) (s.direction == Ascending)) ]

        Nothing ->
            text ""


{-| The sorted column by its visible name, which is what a sighted reader sees
the arrow move to; the key only if no column has it.
-}
sortedLabel : List (Column a msg) -> String -> String
sortedLabel columns key =
    columns
        |> List.find (\col -> col.sortKey == Just key)
        |> Maybe.map .label
        |> Maybe.withDefault key
