module Ui.Separator exposing
    ( labeled
    , line
    , vertical
    )

{-| A line between things.

Three of them, and the difference is what they are made of rather than how they
look. `line` is an `hr`, which is a separator to everything without being told.
`labeled` cannot be — an `hr` holds no text — so it says what it is with a role
and a name, and the words you can see are hidden from anything that reads out
loud, because the name already said them.

-}

import Html exposing (Html, div, hr, span, text)
import Html.Attributes exposing (attribute, class)


{-| A rule between sections of a page.
-}
line : Html msg
line =
    hr [ class "ui-separator" ] []


{-| A rule with something written in the middle of it — "or", "compare with".
-}
labeled : String -> Html msg
labeled label =
    div
        [ class "ui-separator ui-separator--labeled"
        , attribute "role" "separator"
        , attribute "aria-label" label
        ]
        [ span [ class "ui-separator__label", attribute "aria-hidden" "true" ] [ text label ] ]


{-| A rule between things on one line, in a toolbar or a row of metadata.

Presentational: it is there so two groups do not run together, and a screen
reader reading "separator" between every pair of buttons is worse than reading
neither.

-}
vertical : Html msg
vertical =
    span
        [ class "ui-separator ui-separator--vertical"
        , attribute "aria-hidden" "true"
        ]
        []
