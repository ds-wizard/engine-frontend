module Ui.Card exposing (view, viewWithIconTitleLevel, viewWithTitle, viewWithTitleLevel)

{-| A card is a surface, not a box: it separates by elevation and space rather
than by a border. In dark mode the elevation is a lightness step, because
shadows barely read on a dark ground.

**The title is a heading, and its level is the outline's, not the look's.** It
always looks like a subtitle; `viewWithTitle` makes it an `h2`, which is right
for a card that sits directly under the page's `h1`. A card under a section of
its own (an `h2`), or in a dialog, takes `viewWithTitleLevel` with the next
level down — a skipped level leaves a screen reader's list of headings with a
gap that reads as something missing.

-}

import Html exposing (Html, div, h2, h3, h4, h5, h6, span, text)
import Html.Attributes exposing (class)
import Ui.Icon as Icon exposing (Icon)


view : List (Html msg) -> Html msg
view =
    div [ class "ui-card" ]


{-| A card whose title is an `h2`.
-}
viewWithTitle : String -> List (Html msg) -> Html msg
viewWithTitle =
    viewWithTitleLevel 2


{-| A card whose title is a heading of the given level, 2 to 6; anything
outside that range is clamped to it (a card is never the page's `h1`).
-}
viewWithTitleLevel : Int -> String -> List (Html msg) -> Html msg
viewWithTitleLevel level title content =
    div [ class "ui-card" ] (heading level [ class "ui-subtitle" ] [ text title ] :: content)


{-| A card whose title is led by an icon — the mark of the product a card is
about. The icon is decorative: the title already says what it shows.
-}
viewWithIconTitleLevel : Int -> Icon -> String -> List (Html msg) -> Html msg
viewWithIconTitleLevel level icon title content =
    div [ class "ui-card" ]
        (heading level
            [ class "ui-subtitle ui-card__title" ]
            [ Icon.toHtml icon, span [] [ text title ] ]
            :: content
        )


heading : Int -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
heading level =
    case clamp 2 6 level of
        2 ->
            h2

        3 ->
            h3

        4 ->
            h4

        5 ->
            h5

        _ ->
            h6
