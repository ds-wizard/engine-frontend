module Ui.DataGrid exposing
    ( Column
    , DataGrid
    , Model
    , Msg
    , Outcome(..)
    , SortDirection(..)
    , Words
    , column
    , english
    , grid
    , init
    , subscriptions
    , toHtml
    , update
    , withBusy
    , withFill
    , withFiltered
    , withFooter
    , withHeader
    , withHeaderActions
    , withRowHeights
    , withRowNumbers
    , withSort
    , withSortable
    , withTestId
    , withWords
    , withoutResize
    )

{-| A table the reader shapes: columns as wide as they want them, rows as tall,
numbered, with the header kept in view while the rows scroll under it.

`Ui.Table` is for a table that is read. This is for one that is worked in — the
analytics tables, where a view is a set of columns someone chose, sized and
saved — and it is still a real `table`. A screen reader's table commands work
in it, the header cells are `th scope="col"`, the sorted one carries
`aria-sort`, and a change of sort is announced, exactly as in `Ui.Table`.

**Resizing is a splitter, not only a drag.** Each column's trailing edge is the
APG window splitter `Ui.Resizer` is: a focusable `separator` carrying the width
as its value, where the arrow keys narrow and widen the column, Home and End
take it to its limits, and a drag is one way of doing the same thing rather than
the only way (2.1.1, 2.5.7). One tab stop per column, in the header, so the
rows below cost nothing to Tab past.

**Row heights are the exception, and deliberately.** A handle under each row's
number drags that row taller or shorter. It is hidden from assistive technology
and not a tab stop: a separator per row would put a hundred stops between the
header and whatever follows the table. The same result for every row is a
dialog away (`withRowNumbers`' header action), which is what a keyboard reaches
instead — the function is there, the per-row shortcut is the pointer's.

**The grid scrolls in its own region.** Wide tables scroll sideways and long
ones down, inside a `region` named after the table and focusable, so the
keyboard can scroll it too (2.1.1). The header stays at the top of that region,
and the row numbers at its start.

**The caller owns the sizes.** Widths and heights are part of what gets saved
with a view, so they live with the caller; `update` reports each change as an
`Outcome` and the caller stores it. The model holds only the drag in progress.

-}

import Browser.Events
import Html exposing (Html, button, caption, col, colgroup, div, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (attribute, class, classList, scope, style, tabindex, type_)
import Html.Events exposing (onClick, preventDefaultOn)
import Json.Decode as Decode
import List.Extra as List
import Maybe.Extra as Maybe
import Ui.Button as Button exposing (Button)
import Ui.Icon as Icon
import Ui.TestId as TestId
import Ui.Tooltip as Tooltip



-- MODEL


type Model
    = Model
        { bounds : Bounds
        , drag : Maybe Drag
        }


type alias Bounds =
    { minColumnWidth : Int
    , maxColumnWidth : Int
    , minRowHeight : Int
    , maxRowHeight : Int
    }


type Target
    = ColumnEdge String
    | RowEdge String


type alias Drag =
    { target : Target
    , start : Float
    , startSize : Int
    }


{-| The limits every resize is held to, in pixels — the one unit here that is
not rem, for the reason `Ui.Resizer` gives: a size someone dragged is a size
they chose against what was on the screen.
-}
init : { minColumnWidth : Int, maxColumnWidth : Int, minRowHeight : Int, maxRowHeight : Int } -> Model
init bounds =
    Model { bounds = bounds, drag = Nothing }


{-| What changed, for the caller to store. Sent for every step of a drag and for
every key press on a separator; a drag abandoned with Escape reports the size it
started from.
-}
type Outcome
    = ColumnResized String Int
    | RowResized String Int


type Msg
    = ColumnDragStarted String Int Float
    | RowDragStarted String Int Float
    | PointerMoved Float Float
    | PointerReleased
    | DragCanceled
    | ColumnSetTo String Int


update : Msg -> Model -> ( Model, Maybe Outcome )
update msg (Model model) =
    case msg of
        ColumnDragStarted key width x ->
            ( Model { model | drag = Just { target = ColumnEdge key, start = x, startSize = width } }, Nothing )

        RowDragStarted key height y ->
            ( Model { model | drag = Just { target = RowEdge key, start = y, startSize = height } }, Nothing )

        PointerMoved x y ->
            case model.drag of
                Just drag ->
                    case drag.target of
                        ColumnEdge key ->
                            ( Model model
                            , Just (ColumnResized key (clampColumn model.bounds (drag.startSize + round (x - drag.start))))
                            )

                        RowEdge key ->
                            ( Model model
                            , Just (RowResized key (clampRow model.bounds (drag.startSize + round (y - drag.start))))
                            )

                Nothing ->
                    ( Model model, Nothing )

        PointerReleased ->
            ( Model { model | drag = Nothing }, Nothing )

        DragCanceled ->
            case model.drag of
                Just drag ->
                    ( Model { model | drag = Nothing }
                    , case drag.target of
                        ColumnEdge key ->
                            Just (ColumnResized key drag.startSize)

                        RowEdge key ->
                            Just (RowResized key drag.startSize)
                    )

                Nothing ->
                    ( Model model, Nothing )

        ColumnSetTo key width ->
            ( Model model, Just (ColumnResized key (clampColumn model.bounds width)) )


clampColumn : Bounds -> Int -> Int
clampColumn bounds =
    clamp bounds.minColumnWidth bounds.maxColumnWidth


clampRow : Bounds -> Int -> Int
clampRow bounds =
    clamp bounds.minRowHeight bounds.maxRowHeight


{-| While a drag is in progress: the pointer's movement, its release, and
Escape, which puts the size back where it started.
-}
subscriptions : Model -> Sub Msg
subscriptions (Model model) =
    case model.drag of
        Just _ ->
            Sub.batch
                [ Browser.Events.onMouseMove
                    (Decode.map2 PointerMoved
                        (Decode.field "clientX" Decode.float)
                        (Decode.field "clientY" Decode.float)
                    )
                , Browser.Events.onMouseUp (Decode.succeed PointerReleased)
                , Browser.Events.onKeyDown
                    (Decode.field "key" Decode.string
                        |> Decode.andThen
                            (\key ->
                                if key == "Escape" then
                                    Decode.succeed DragCanceled

                                else
                                    Decode.fail "not escape"
                            )
                    )
                ]

        Nothing ->
            Sub.none



-- COLUMNS


type Column a msg
    = Column
        { key : String
        , label : String
        , width : Int
        , render : a -> Html msg
        , sortable : Bool
        , filtered : Bool
        , header : Maybe (List (Html msg))
        , actions : List (Html msg)
        , resizable : Bool
        }


{-| One column: `key` is what sorting and resizing report it by, `width` is its
width now, in pixels.
-}
column : { key : String, label : String, width : Int, render : a -> Html msg } -> Column a msg
column config =
    Column
        { key = config.key
        , label = config.label
        , width = config.width
        , render = config.render
        , sortable = False
        , filtered = False
        , header = Nothing
        , actions = []
        , resizable = True
        }


{-| The header becomes a button that asks for this column to be sorted by
(`withSort`'s `onSort`); the grid only says which column is sorted, the caller
decides what a second press means.
-}
withSortable : Column a msg -> Column a msg
withSortable (Column config) =
    Column { config | sortable = True }


{-| The column is narrowed by a filter. Shown as a glyph in the header, and said
in words to a screen reader, so a table that shows fewer rows than expected
says which column is why.
-}
withFiltered : Bool -> Column a msg -> Column a msg
withFiltered filtered (Column config) =
    Column { config | filtered = filtered }


{-| The header holds controls instead of the label — a column whose name is
edited in place, say. The label is still the header's name for a screen reader
and for the resize handle, so a cell is announced with its column.
-}
withHeader : List (Html msg) -> Column a msg -> Column a msg
withHeader content (Column config) =
    Column { config | header = Just content }


{-| Controls after the label, at the header cell's end edge — the column's own
filter (`Ui.FilterMenu.toHeaderHtml`). The label still sorts and still names
the column; the controls are tab stops of their own after it.
-}
withHeaderActions : List (Html msg) -> Column a msg -> Column a msg
withHeaderActions actions (Column config) =
    Column { config | actions = actions }


{-| No resize handle: a column of controls the width of its buttons — a row's
actions — has nothing to make wider.
-}
withoutResize : Column a msg -> Column a msg
withoutResize (Column config) =
    Column { config | resizable = False }



-- GRID


type SortDirection
    = Ascending
    | Descending


type DataGrid a msg
    = DataGrid
        { id : String
        , label : String
        , columns : List (Column a msg)
        , rows : List a
        , rowKey : a -> String
        , toMsg : Msg -> msg
        , model : Model
        , sort : Maybe { key : Maybe String, direction : SortDirection, onSort : String -> msg }
        , rowNumbers : Maybe { start : Int, headerAction : Maybe (Button msg) }
        , rowHeight : Maybe (a -> Int)
        , busy : Bool
        , words : Words
        , testId : Maybe String
        , footer : Maybe (Html msg)
        , fill : Bool
        }


{-| What the grid says of its own accord.

  - `sort` — the tooltip on an unsorted column's sort glyph, given its label.
  - `sorted` — the sort announcement, given the column's label and whether the
    order is ascending; also the tooltip on the sorted column's glyph.
  - `rowNumber` — the name of the row-number column.
  - `filtered` — what the filter glyph in a header says.
  - `resizeColumn` — a column separator's name, given the column's label.
  - `width` — a separator's value in words, given the width in pixels.

-}
type alias Words =
    { sort : String -> String
    , sorted : String -> Bool -> String
    , rowNumber : String
    , filtered : String
    , resizeColumn : String -> String
    , width : Int -> String
    }


english : Words
english =
    { sort = \label -> "Sort by " ++ label
    , sorted =
        \label ascending ->
            "Sorted by "
                ++ label
                ++ ", "
                ++ (if ascending then
                        "ascending"

                    else
                        "descending"
                   )
    , rowNumber = "Row"
    , filtered = "filtered"
    , resizeColumn = \label -> "Resize column " ++ label
    , width = \pixels -> String.fromInt pixels ++ " pixels"
    }


{-| `id` must be unique on the page; `label` names the table and the region it
scrolls in — "Projects", the page's own subject.
-}
grid :
    { id : String
    , label : String
    , columns : List (Column a msg)
    , rows : List a
    , rowKey : a -> String
    , toMsg : Msg -> msg
    , model : Model
    }
    -> DataGrid a msg
grid config =
    DataGrid
        { id = config.id
        , label = config.label
        , columns = config.columns
        , rows = config.rows
        , rowKey = config.rowKey
        , toMsg = config.toMsg
        , model = config.model
        , sort = Nothing
        , rowNumbers = Nothing
        , rowHeight = Nothing
        , busy = False
        , words = english
        , testId = Nothing
        , footer = Nothing
        , fill = False
        }


{-| Which column the rows are sorted by, if any, and what a press on a sortable
column's header asks for. The grid does not decide what a second press means —
the other direction, or no order at all — the caller does, in `onSort`.
-}
withSort : { key : Maybe String, direction : SortDirection, onSort : String -> msg } -> DataGrid a msg -> DataGrid a msg
withSort sort (DataGrid config) =
    DataGrid { config | sort = Just sort }


{-| A first column numbering the rows from `start` — the position in the whole
result, not on the page. Its header can hold one action: the dialog that sets
every row's height, which is the keyboard's way to what the row handles do.
-}
withRowNumbers : { start : Int, headerAction : Maybe (Button msg) } -> DataGrid a msg -> DataGrid a msg
withRowNumbers rowNumbers (DataGrid config) =
    DataGrid { config | rowNumbers = Just rowNumbers }


{-| What follows the last row, inside the grid's scrolling region — the way to
add another row, under the rows it adds to. It keeps to the region's start edge
while the rows scroll sideways under it.
-}
withFooter : Html msg -> DataGrid a msg -> DataGrid a msg
withFooter footer (DataGrid config) =
    DataGrid { config | footer = Just footer }


{-| Each row as tall as the caller says, clipped to it, with a handle to drag it
taller or shorter (see the module's notes on why the handle is the pointer's
alone). Needs `withRowNumbers`: the handle lives in the number cell.
-}
withRowHeights : (a -> Int) -> DataGrid a msg -> DataGrid a msg
withRowHeights heightOf (DataGrid config) =
    DataGrid { config | rowHeight = Just heightOf }


{-| The rows are being replaced: `aria-busy` while they are, a dimmed body, so
the old rows are not read as the answer to the new question, and the system's
spinner over them once it has taken long enough to notice.
-}
withBusy : Bool -> DataGrid a msg -> DataGrid a msg
withBusy busy (DataGrid config) =
    DataGrid { config | busy = busy }


{-| The grid is as tall as its container, edge to edge and with no frame of its
own: the sheet is the page, a toolbar above it and a footer below, and the rows
scroll between them. The container has to give it a height — a flex or grid
cell that is told to fill what is left.
-}
withFill : DataGrid a msg -> DataGrid a msg
withFill (DataGrid config) =
    DataGrid { config | fill = True }


withWords : Words -> DataGrid a msg -> DataGrid a msg
withWords words (DataGrid config) =
    DataGrid { config | words = words }


withTestId : String -> DataGrid a msg -> DataGrid a msg
withTestId testId (DataGrid config) =
    DataGrid { config | testId = Just testId }



-- VIEW


toHtml : DataGrid a msg -> Html msg
toHtml (DataGrid config) =
    let
        (Model model) =
            config.model

        numberColumn =
            case config.rowNumbers of
                Just _ ->
                    [ col [ class "ui-data-grid__number-col" ] [] ]

                Nothing ->
                    []

        widths =
            List.map (\(Column c) -> c.width) config.columns

        dragging =
            Maybe.isJust model.drag
    in
    div [ classList [ ( "ui-data-grid-wrapper", True ), ( "ui-data-grid-wrapper--fill", config.fill ) ] ]
        [ div
            ([ classList
                [ ( "ui-data-grid", True )
                , ( "is-busy", config.busy )
                , ( "is-dragging", dragging )
                ]
             , Html.Attributes.id config.id
             , attribute "role" "region"
             , attribute "aria-label" config.label
             , tabindex 0
             ]
                ++ TestId.attributes config.testId
            )
            [ table
                [ class "ui-data-grid__table"
                , style "--ui-data-grid-width" (String.fromInt (List.sum widths) ++ "px")
                , attribute "aria-busy"
                    (if config.busy then
                        "true"

                     else
                        "false"
                    )
                ]
                [ caption [ class "ui-visually-hidden" ] [ text config.label ]
                , colgroup []
                    (numberColumn
                        ++ List.map (\width -> col [ style "inline-size" (String.fromInt width ++ "px") ] []) widths
                    )
                , thead []
                    [ tr []
                        (viewNumberHead config
                            ++ List.map (viewHead config model.bounds) config.columns
                        )
                    ]
                , tbody [] (List.indexedMap (viewRow config) config.rows)
                ]
            , case config.footer of
                Just footer ->
                    div [ class "ui-data-grid__footer" ] [ footer ]

                Nothing ->
                    text ""
            ]
        , if config.busy then
            -- Visual: aria-busy on the table already says it.
            div [ class "ui-data-grid__busy", attribute "aria-hidden" "true" ] [ Icon.toHtml Icon.spinner ]

          else
            text ""
        , announcement config
        ]


viewNumberHead : { a | rowNumbers : Maybe { start : Int, headerAction : Maybe (Button msg) }, words : Words } -> List (Html msg)
viewNumberHead config =
    case config.rowNumbers of
        Just rowNumbers ->
            [ th [ scope "col", class "ui-data-grid__th ui-data-grid__th--number" ]
                [ span [ class "ui-visually-hidden" ] [ text config.words.rowNumber ]
                , case rowNumbers.headerAction of
                    Just action ->
                        -- The corner is at the grid's start edge, next to the navigation: the
                        -- bubble opens from there over the grid, not centered and under it.
                        Button.toHtml (Button.small (Button.withTooltipAt Tooltip.Start action))

                    Nothing ->
                        text ""
                ]
            ]

        Nothing ->
            []


viewHead :
    { a
        | sort : Maybe { key : Maybe String, direction : SortDirection, onSort : String -> msg }
        , words : Words
        , toMsg : Msg -> msg
        , model : Model
    }
    -> Bounds
    -> Column b msg
    -> Html msg
viewHead config bounds (Column c) =
    let
        activeDirection =
            config.sort
                |> Maybe.andThen
                    (\sort ->
                        if sort.key == Just c.key then
                            Just sort.direction

                        else
                            Nothing
                    )

        sortAttribute =
            case activeDirection of
                Just Ascending ->
                    [ attribute "aria-sort" "ascending" ]

                Just Descending ->
                    [ attribute "aria-sort" "descending" ]

                Nothing ->
                    []

        filtered =
            if c.filtered then
                [ span [ class "ui-data-grid__filtered" ]
                    [ Icon.toHtml (Icon.solid "fa-filter")
                    , span [ class "ui-visually-hidden" ] [ text (" (" ++ config.words.filtered ++ ")") ]
                    ]
                ]

            else
                []

        label =
            case ( c.header, c.sortable, config.sort ) of
                ( Just content, _, _ ) ->
                    span [ class "ui-data-grid__heading ui-data-grid__heading--controls" ]
                        (span [ class "ui-visually-hidden" ] [ text c.label ] :: content)

                ( Nothing, True, Just sort ) ->
                    let
                        -- What the glyph means, said on hover: visual only, as the
                        -- button's own name and aria-sort already carry it.
                        sortGlyph =
                            Tooltip.view
                                { label =
                                    case activeDirection of
                                        Just direction ->
                                            config.words.sorted c.label (direction == Ascending)

                                        Nothing ->
                                            config.words.sort c.label
                                , placement = Tooltip.Below
                                }
                                (span [ class "ui-data-grid__sort-glyph", attribute "aria-hidden" "true" ] [])
                    in
                    button
                        [ type_ "button"
                        , class "ui-data-grid__sort"
                        , onClick (sort.onSort c.key)
                        , TestId.attribute "header_sort"
                        ]
                        ((span [ class "ui-data-grid__label" ] [ text c.label ] :: filtered) ++ [ sortGlyph ])

                _ ->
                    span [ class "ui-data-grid__heading" ]
                        (span [ class "ui-data-grid__label" ] [ text c.label ] :: filtered)
    in
    th
        ([ scope "col"
         , class "ui-data-grid__th"
         , TestId.attribute ("header-" ++ c.key)
         ]
            ++ sortAttribute
        )
        [ if List.isEmpty c.actions then
            label

          else
            div [ class "ui-data-grid__head" ]
                [ label
                , div [ class "ui-data-grid__actions" ] c.actions
                ]
        , if c.resizable then
            viewColumnSeparator config bounds c

          else
            text ""
        ]


{-| The column's trailing edge, as a splitter. See `Ui.Resizer` for why the
press is taken over and why that is allowed.
-}
viewColumnSeparator : { a | words : Words, toMsg : Msg -> msg, model : Model } -> Bounds -> { b | key : String, label : String, width : Int } -> Html msg
viewColumnSeparator config bounds c =
    let
        (Model model) =
            config.model

        active =
            case model.drag of
                Just drag ->
                    drag.target == ColumnEdge c.key

                Nothing ->
                    False

        step =
            16
    in
    div
        [ classList [ ( "ui-data-grid__resize", True ), ( "is-dragging", active ) ]
        , attribute "role" "separator"
        , attribute "aria-orientation" "vertical"
        , attribute "aria-label" (config.words.resizeColumn c.label)
        , attribute "aria-valuenow" (String.fromInt c.width)
        , attribute "aria-valuemin" (String.fromInt bounds.minColumnWidth)
        , attribute "aria-valuemax" (String.fromInt bounds.maxColumnWidth)
        , attribute "aria-valuetext" (config.words.width c.width)
        , tabindex 0
        , -- WCAG 2.5.2 down-event: a drag has no other moment to begin. Nothing
          -- completes on the press, and Escape puts the width back.
          preventDefaultOn "mousedown"
            (Decode.map (\x -> ( config.toMsg (ColumnDragStarted c.key c.width x), True )) primaryButton)
        , preventDefaultOn "keydown"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        case key of
                            "ArrowLeft" ->
                                Decode.succeed (c.width - step)

                            "ArrowRight" ->
                                Decode.succeed (c.width + step)

                            "Home" ->
                                Decode.succeed bounds.minColumnWidth

                            "End" ->
                                Decode.succeed bounds.maxColumnWidth

                            _ ->
                                Decode.fail "not a resize key"
                    )
                |> Decode.map (\width -> ( config.toMsg (ColumnSetTo c.key width), True ))
            )
        ]
        []


primaryButton : Decode.Decoder Float
primaryButton =
    Decode.field "button" Decode.int
        |> Decode.andThen
            (\pressed ->
                if pressed == 0 then
                    Decode.field "clientX" Decode.float

                else
                    Decode.fail "not the primary button"
            )


primaryButtonY : Decode.Decoder Float
primaryButtonY =
    Decode.field "button" Decode.int
        |> Decode.andThen
            (\pressed ->
                if pressed == 0 then
                    Decode.field "clientY" Decode.float

                else
                    Decode.fail "not the primary button"
            )


viewRow :
    { a
        | columns : List (Column b msg)
        , rowKey : b -> String
        , rowNumbers : Maybe { start : Int, headerAction : Maybe (Button msg) }
        , rowHeight : Maybe (b -> Int)
        , toMsg : Msg -> msg
        , model : Model
    }
    -> Int
    -> b
    -> Html msg
viewRow config index row =
    let
        heightAttributes =
            case config.rowHeight of
                Just heightOf ->
                    [ style "--ui-data-grid-row-size" (String.fromInt (heightOf row) ++ "px") ]

                Nothing ->
                    []

        numberCell =
            case config.rowNumbers of
                Just rowNumbers ->
                    let
                        handle =
                            case config.rowHeight of
                                Just heightOf ->
                                    let
                                        (Model model) =
                                            config.model

                                        key =
                                            config.rowKey row

                                        active =
                                            case model.drag of
                                                Just drag ->
                                                    drag.target == RowEdge key

                                                Nothing ->
                                                    False
                                    in
                                    -- Hidden and not a stop: the height of every row is set from
                                    -- the number column's header, which the keyboard reaches.
                                    div
                                        [ classList [ ( "ui-data-grid__row-handle", True ), ( "is-dragging", active ) ]
                                        , attribute "aria-hidden" "true"

                                        -- WCAG 2.5.2 down-event: a drag has no other moment to
                                        -- begin. Escape puts the height back.
                                        , preventDefaultOn "mousedown"
                                            (Decode.map (\y -> ( config.toMsg (RowDragStarted key (heightOf row) y), True )) primaryButtonY)
                                        ]
                                        []

                                Nothing ->
                                    text ""
                    in
                    [ td [ class "ui-data-grid__td ui-data-grid__td--number" ]
                        [ text (String.fromInt (rowNumbers.start + index))
                        , handle
                        ]
                    ]

                Nothing ->
                    []
    in
    tr
        (classList [ ( "ui-data-grid__row", True ), ( "ui-data-grid__row--sized", Maybe.isJust config.rowHeight ) ]
            :: heightAttributes
        )
        (numberCell
            ++ List.map
                (\(Column c) ->
                    td [ class "ui-data-grid__td", TestId.attribute ("field-" ++ c.key) ]
                        [ div [ class "ui-data-grid__cell" ] [ c.render row ] ]
                )
                config.columns
        )


announcement : { a | sort : Maybe { key : Maybe String, direction : SortDirection, onSort : String -> msg }, columns : List (Column b msg), words : Words } -> Html msg
announcement config =
    div
        [ class "ui-visually-hidden"
        , attribute "aria-live" "polite"
        ]
        [ case config.sort of
            Just sort ->
                let
                    label =
                        config.columns
                            |> List.find (\(Column c) -> Just c.key == sort.key)
                            |> Maybe.map (\(Column c) -> c.label)
                in
                case label of
                    Just columnLabel ->
                        text (config.words.sorted columnLabel (sort.direction == Ascending))

                    Nothing ->
                        text ""

            Nothing ->
                text ""
        ]
