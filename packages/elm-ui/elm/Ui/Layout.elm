module Ui.Layout exposing
    ( center
    , container
    , grid
    , inline
    , inlineBetween
    , inlineEnd
    , split
    , stack
    , stackLoose
    )

{-| Layout primitives named for intent rather than arithmetic.

Spacing is `gap` on the container, never margins on children: that removes
margin collapse, first- and last-child special cases, and the double gap where
two spaced components meet.

`container` opts a subtree into container queries, so a component sizes to the
space it is given rather than to the viewport. A card in a 350px panel reads as
narrow on any screen.

-}

import Html exposing (Html, div)
import Html.Attributes exposing (class)


stack : List (Html msg) -> Html msg
stack =
    div [ class "ui-stack" ]


{-| `stack`, for parts that are separate things rather than a list of like ones
— the button that acts on a table, above the table.
-}
stackLoose : List (Html msg) -> Html msg
stackLoose =
    div [ class "ui-stack ui-stack--loose" ]


inline : List (Html msg) -> Html msg
inline =
    div [ class "ui-inline" ]


inlineBetween : List (Html msg) -> Html msg
inlineBetween =
    div [ class "ui-inline ui-inline--between" ]


{-| A row pushed to the inline end — a section's own action above what it acts
on, say. The end, not the right: it follows the writing direction.
-}
inlineEnd : List (Html msg) -> Html msg
inlineEnd =
    div [ class "ui-inline ui-inline--end" ]


grid : List (Html msg) -> Html msg
grid =
    div [ class "ui-grid" ]


{-| Two areas side by side, the first twice the width of the second: the content
and what stands beside it. Give it the content first — the profile form, then
the user's picture. Too narrow for both and they stack, content first still.
-}
split : List (Html msg) -> Html msg
split =
    div [ class "ui-split" ]


center : List (Html msg) -> Html msg
center =
    div [ class "ui-center" ]


container : List (Html msg) -> Html msg
container =
    div [ class "ui-container" ]
