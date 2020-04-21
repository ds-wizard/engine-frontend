module Shared.Elemental.Utils exposing (colorD05, colorD10, colorL10, colorL20, colorL40, contrastRatio, px2rem)

import Color
import Color.Accessibility as Color
import Color.Convert exposing (colorToHex, hexToColor)
import Color.Manipulate exposing (darken, lighten)
import Css exposing (Color, Rem, hex, rem)


px2rem : Float -> Rem
px2rem size =
    rem <| size / 16


colorL10 : Color -> Color
colorL10 =
    manipulate lighten 0.1


colorL20 : Color -> Color
colorL20 =
    manipulate lighten 0.2


colorL40 : Color -> Color
colorL40 =
    manipulate lighten 0.4


colorD05 : Color -> Color
colorD05 =
    manipulate darken 0.05


colorD10 : Color -> Color
colorD10 =
    manipulate darken 0.1


manipulate : (Float -> Color.Color -> Color.Color) -> Float -> Color -> Color
manipulate fn amount inputColor =
    hexToColor inputColor.value
        |> Result.map (hex << colorToHex << fn amount)
        |> Result.withDefault inputColor


contrastRatio : Color -> Color -> Float
contrastRatio color1 color2 =
    case ( hexToColor color1.value, hexToColor color2.value ) of
        ( Ok c1, Ok c2 ) ->
            Color.contrastRatio c1 c2

        _ ->
            0
