module Shared.Elemental.Atoms.Advice exposing (view)

import Css exposing (..)
import Css.Global exposing (descendants, typeSelector)
import Css.Transitions exposing (transition)
import Html.Styled exposing (Html, div, fromUnstyled, span, text)
import Html.Styled.Attributes exposing (css)
import Markdown
import Shared.Elemental.Foundations.Border as Border
import Shared.Elemental.Foundations.Grid as Grid
import Shared.Elemental.Foundations.Illustration as Illustration
import Shared.Elemental.Foundations.Spacing as Spacing
import Shared.Elemental.Foundations.Transition as Transition
import Shared.Elemental.Foundations.Typography as Typography
import Shared.Elemental.Theme exposing (Theme)
import Shared.Elemental.Utils exposing (px2rem)


view : Theme -> Html msg
view theme =
    let
        adviceText =
            "Maecenas at lorem erat. Nulla nec aliquet turpis, ac mattis nisi. Donec quis ligula vel mi dapibus dignissim. Pellentesque commodo lectus sit amet nulla volutpat pharetra.\n\nCurabitur convallis porttitor placerat. Duis sagittis viverra sollicitudin."

        styles =
            [ Typography.copy1lighter theme
            , Spacing.insetSquishLG
            , Spacing.stackLG
            , Border.default theme
            , position relative
            , descendants
                [ typeSelector "p"
                    [ Spacing.stackMD ]
                , typeSelector "svg"
                    [ maxHeight (px2rem 150) ]
                ]
            ]

        markdown =
            fromUnstyled << Markdown.toHtml []

        grid =
            Grid.comfortable
    in
    div [ css styles ]
        [ grid.row []
            [ grid.col 3 [] [ Illustration.buildWireframes theme ]
            , grid.col 9 [ Grid.colVerticalCenter ] [ markdown adviceText ]
            ]
        , closeButton theme
        ]


closeButton : Theme -> Html msg
closeButton theme =
    let
        styles =
            [ Typography.copy1 theme
            , Spacing.insetSquishSM
            , position absolute
            , right zero
            , top zero
            , cursor pointer
            , transition
                [ Transition.default Css.Transitions.color3
                ]
            , hover
                [ color theme.colors.primary
                ]
            ]
    in
    span [ css styles ] [ text "×" ]
