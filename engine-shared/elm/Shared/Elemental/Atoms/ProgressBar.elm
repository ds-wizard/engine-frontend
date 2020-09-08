module Shared.Elemental.Atoms.ProgressBar exposing (..)

import Color.Convert exposing (colorToHex, hexToColor)
import Color.Interpolate exposing (Space(..), interpolate)
import Css exposing (absolute, backgroundColor, bottom, display, height, hex, hidden, inlineBlock, left, overflow, pct, position, relative, top, width, zero)
import Css.Global exposing (descendants, typeSelector)
import Html.Styled as Html exposing (Html, span)
import Html.Styled.Attributes exposing (css)
import Shared.Elemental.Foundations.Border as Border
import Shared.Elemental.Theme exposing (Theme)
import Shared.Elemental.Utils exposing (px2rem)


default : Theme -> Float -> List (Html.Attribute msg) -> Html msg
default theme value attributes =
    let
        fillColor =
            case ( hexToColor theme.colors.danger.value, hexToColor theme.colors.success.value ) of
                ( Ok c1, Ok c2 ) ->
                    hex <| colorToHex <| interpolate HSL c1 c2 value

                _ ->
                    theme.colors.primary

        styles =
            [ Border.roundedFull
            , width (pct 100)
            , height (px2rem 10)
            , position relative
            , display inlineBlock
            , backgroundColor theme.colors.backgroundTint
            , overflow hidden
            , descendants
                [ typeSelector "span"
                    [ position absolute
                    , left zero
                    , top zero
                    , bottom zero
                    , width (pct (100 * value))
                    , backgroundColor fillColor
                    ]
                ]
            ]
    in
    span (css styles :: attributes)
        [ span [] []
        ]
