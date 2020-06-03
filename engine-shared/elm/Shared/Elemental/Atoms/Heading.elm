module Shared.Elemental.Atoms.Heading exposing (h_)

import Css exposing (..)
import Html.Styled as Html exposing (Html, text)
import Html.Styled.Attributes exposing (css)
import Shared.Elemental.Foundations.Spacing as Spacing
import Shared.Elemental.Foundations.Typography as Typography
import Shared.Elemental.Theme exposing (Theme)


h1 : Theme -> String -> Html msg
h1 theme =
    h_ Html.h1
        [ Typography.heading1 theme
        , Spacing.stackMD
        ]


h2 : Theme -> String -> Html msg
h2 theme =
    h_ Html.h2
        [ Typography.heading2 theme
        , Spacing.stackMD
        ]


h3 : Theme -> String -> Html msg
h3 theme =
    h_ Html.h3
        [ Typography.heading3 theme
        , Spacing.stackSM
        ]


h_ : (List (Html.Attribute msg) -> List (Html msg) -> Html msg) -> List Style -> String -> Html msg
h_ node styles headingText =
    node [ css styles ] [ text headingText ]
