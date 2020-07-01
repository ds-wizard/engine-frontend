module Shared.Elemental.Atoms.Badge exposing (..)

import Html.Styled as Html exposing (Html, span)
import Html.Styled.Attributes exposing (css)
import Shared.Elemental.Foundations.Border as Border
import Shared.Elemental.Foundations.Spacing as Spacing
import Shared.Elemental.Foundations.Typography as Typography
import Shared.Elemental.Theme exposing (Theme)


outline : Theme -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
outline theme attributes content =
    let
        styles =
            [ Typography.copy2lighter theme
            , Border.default theme
            , Spacing.insetSquishXS
            ]
    in
    span (css styles :: attributes) content
