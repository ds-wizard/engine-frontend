module Ui.TreeTable exposing
    ( Row
    , TreeTable
    , row
    , toHtml
    , treeTable
    , withFill
    , withIcon
    , withSelected
    , withTestId
    , withToggle
    )

{-| A table whose rows are a tree: a knowledge model's chapters, their
questions, a question's answers and what follows them, each row with its
numbers beside it.

It is a `table`, not the ARIA `treegrid`. A treegrid is one tab stop moved
through with the arrow keys in two dimensions, which is a great deal of
interaction to learn for what is read, not edited, and which no reader of a
report expects. Here the structure is carried by what is in the first column
of each row, which is its header (`th scope="row"`): the row's name, indented
by its depth, and — for a row with rows under it — a disclosure button that
says whether they are shown (`aria-expanded`) and whose they are ("Collapse
Chapter 1"). Tab moves through the disclosures and whatever the caller put in
the names; a screen reader's table commands move through the rest.

**The depth is said as well as drawn.** Indentation is invisible to a screen
reader, so a row below the top carries its level in words, visually hidden,
before its name (`Words.level`).

**A row can be the one something beside the table is about** — the question
whose details a panel shows. `withSelected` marks it with a tint and says so
(`aria-current`), since a tint alone is not something a screen reader reads.

**Filling a region** (`withFill`), the table is the whole of it, as the
knowledge model editor's tag and phase matrices are: a grid of ruled cells
that scrolls itself both ways, the header row and the names' column staying
where they are, each marked off by a heavier rule, and the top rows — the
chapters — on the sunken ground, as the groups the rest is in.

-}

import Html exposing (Html, button, caption, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (attribute, class, classList, scope, style, type_)
import Html.Events exposing (onClick)
import Ui.Icon as Icon exposing (Icon)
import Ui.TestId as TestId


type TreeTable msg
    = TreeTable
        { label : String
        , columns : List String
        , rows : List (Row msg)
        , level : Int -> String
        , testId : Maybe String
        , fill : Bool
        }


type Row msg
    = Row
        { key : String
        , level : Int
        , name : Html msg
        , cells : List (Html msg)
        , toggle : Maybe { expanded : Bool, onToggle : msg, label : String }
        , selected : Bool
        , testId : Maybe String
        , icon : Maybe Icon
        }


{-| `label` names the table (its caption, visually hidden); `columns` are the
headers, the first of them the tree's own. `level` says a row's depth in words
— "level 2" — for rows below the top.
-}
treeTable : { label : String, columns : List String, rows : List (Row msg), level : Int -> String } -> TreeTable msg
treeTable config =
    TreeTable
        { label = config.label
        , columns = config.columns
        , rows = config.rows
        , level = config.level
        , testId = Nothing
        , fill = False
        }


withTestId : String -> TreeTable msg -> TreeTable msg
withTestId testId (TreeTable config) =
    TreeTable { config | testId = Just testId }


{-| The table is the region it is in: edge to edge, as tall as the region, and
scrolling itself, with the header and the names staying in view. For a table
that is a page's whole work — in a flush `Ui.Page.Workspace`.
-}
withFill : TreeTable msg -> TreeTable msg
withFill (TreeTable config) =
    TreeTable { config | fill = True }


{-| A row: its depth from 1, its name — text, or a button that opens something
about it — and a cell for each column after the first.
-}
row : { key : String, level : Int, name : Html msg, cells : List (Html msg) } -> Row msg
row config =
    Row
        { key = config.key
        , level = config.level
        , name = config.name
        , cells = config.cells
        , toggle = Nothing
        , selected = False
        , testId = Nothing
        , icon = Nothing
        }


{-| The rows under this one can be hidden and shown. `label` is the button's
name and should say whose rows they are — "Collapse Administrative".
-}
withToggle : { expanded : Bool, onToggle : msg, label : String } -> Row msg -> Row msg
withToggle toggle (Row config) =
    Row { config | toggle = Just toggle }


{-| What kind of thing the row is — a chapter, a question, an answer — as a
glyph before its name, in the subtle ink the knowledge model editor's tree and
matrices draw theirs in. Decorative: the name says what it is.
-}
withIcon : Icon -> Row msg -> Row msg
withIcon icon (Row config) =
    Row { config | icon = Just icon }


withSelected : Bool -> Row msg -> Row msg
withSelected selected (Row config) =
    Row { config | selected = selected }


toHtml : TreeTable msg -> Html msg
toHtml (TreeTable config) =
    Html.div
        (classList [ ( "ui-tree-table-wrapper", True ), ( "ui-tree-table-wrapper--fill", config.fill ) ]
            :: TestId.attributes config.testId
        )
        [ table [ class "ui-tree-table" ]
            [ caption [ class "ui-visually-hidden" ] [ text config.label ]
            , thead []
                [ tr [] (List.map (\label -> th [ scope "col", class "ui-tree-table__th" ] [ text label ]) config.columns) ]
            , tbody [] (List.map (viewRow config.level) config.rows)
            ]
        ]


viewRow : (Int -> String) -> Row msg -> Html msg
viewRow levelWords (Row config) =
    let
        toggle =
            case config.toggle of
                Just disclosure ->
                    button
                        [ type_ "button"
                        , class "ui-tree-table__toggle"
                        , attribute "aria-expanded"
                            (if disclosure.expanded then
                                "true"

                             else
                                "false"
                            )
                        , attribute "aria-label" disclosure.label
                        , onClick disclosure.onToggle
                        ]
                        [ Icon.toHtml
                            (Icon.solid
                                (if disclosure.expanded then
                                    "fa-caret-down"

                                 else
                                    "fa-caret-right"
                                )
                            )
                        ]

                Nothing ->
                    span [ class "ui-tree-table__spacer" ] []

        level =
            if config.level > 1 then
                span [ class "ui-visually-hidden" ] [ text (levelWords config.level ++ " ") ]

            else
                text ""
    in
    tr
        (classList [ ( "ui-tree-table__row", True ), ( "is-selected", config.selected ), ( "ui-tree-table__row--top", config.level <= 1 ) ]
            :: (if config.selected then
                    [ attribute "aria-current" "true" ]

                else
                    []
               )
        )
        (th
            [ scope "row"
            , class "ui-tree-table__name"
            , style "--ui-tree-table-level" (String.fromInt (max 0 (config.level - 1)))
            ]
            [ span [ class "ui-tree-table__name-inner" ]
                [ toggle
                , case config.icon of
                    Just icon ->
                        span [ class "ui-tree-table__icon" ] [ Icon.toHtml icon ]

                    Nothing ->
                        text ""
                , level
                , config.name
                ]
            ]
            :: List.map (\cell -> td [ class "ui-tree-table__td" ] [ cell ]) config.cells
        )
