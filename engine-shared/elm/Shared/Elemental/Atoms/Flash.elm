module Shared.Elemental.Atoms.Flash exposing (danger)

import Css exposing (..)
import Css.Global exposing (class, descendants)
import Html.Styled exposing (Html, div, text)
import Html.Styled.Attributes exposing (css)
import Shared.Elemental.Foundations.Border as Border
import Shared.Elemental.Foundations.Spacing as Spacing
import Shared.Elemental.Foundations.Typography as Typography
import Shared.Elemental.Theme exposing (Theme)
import Shared.Elemental.Utils exposing (colorL40)
import Shared.Html.Styled exposing (fa)


danger : Theme -> String -> Html msg
danger theme content =
    let
        styles =
            [ Typography.copy1danger theme
            , Spacing.stackMD
            , Spacing.insetSquishMD
            , Border.roundedDefault
            , textAlign left
            , backgroundColor (colorL40 theme.colors.danger)
            , descendants
                [ class "fa"
                    [ Spacing.inlineSM
                    ]
                ]
            ]
    in
    div [ css styles ] [ fa "fas fa-exclamation-circle", text content ]
