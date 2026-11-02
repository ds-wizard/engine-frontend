module Ui.ButtonGroup exposing (toHtml)

{-| Controls that belong together, joined into one.

The sort control is the reason it exists: a menu naming what the list is ordered
by, next to a button that flips the direction. Two separate buttons say two
separate things; joined, they say one thing with two parts, which is what they
are.

It is a `group` with a name, not a toolbar and not a radio group — the parts
keep their own roles and their own keyboard behavior, and the group only says
that they are related. A screen reader announces the name once on the way in
rather than repeating "sort" on each button.

-}

import Html exposing (Html, div)
import Html.Attributes exposing (attribute, class)


toHtml : { label : String, items : List (Html msg) } -> Html msg
toHtml config =
    div
        [ class "ui-button-group"
        , attribute "role" "group"
        , attribute "aria-label" config.label
        ]
        config.items
