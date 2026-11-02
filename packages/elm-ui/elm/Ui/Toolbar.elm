module Ui.Toolbar exposing (toHtml)

{-| The row above a listing: a search, and the controls that narrow or order
what the search returns.

It is a named group, not an ARIA `toolbar`, and the difference is the keyboard.
`role="toolbar"` promises the APG toolbar pattern — one tab stop, arrow keys
between the controls — and a screen reader announces it that way. The controls
here are independent widgets, each already one stop of its own: a text field,
where the arrow keys belong to the caret, and menu buttons, where Down opens the
menu. Folding them into a roving tabindex would take the arrow keys away from
both. So Tab walks the row, which is what every control in it expects, and
`role="group"` with the label says what the row is for without promising a
keyboard model it does not have.

The label is required. A group without a name is a `div` to assistive
technology, and "Filter knowledge models" is what makes the row findable from a
screen reader's list of regions and groups.

The layout owns the responsive behavior: the search grows into the row and the
controls wrap beside it, and when the toolbar itself is narrow — by its own
width, not the window's — the two stack, search first. Visual order is DOM order
throughout, so the Tab order is the reading order.

-}

import Html exposing (Html, div)
import Html.Attributes exposing (attribute, class)


toHtml : { label : String, search : Maybe (Html msg), controls : List (Html msg) } -> Html msg
toHtml config =
    div
        [ class "ui-toolbar"
        , attribute "role" "group"
        , attribute "aria-label" config.label
        ]
        [ div [ class "ui-toolbar__row" ]
            [ case config.search of
                Just search ->
                    div [ class "ui-toolbar__search" ] [ search ]

                Nothing ->
                    Html.text ""
            , if List.isEmpty config.controls then
                Html.text ""

              else
                div [ class "ui-toolbar__controls" ] config.controls
            ]
        ]
