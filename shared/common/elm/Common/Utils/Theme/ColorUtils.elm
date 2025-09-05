module Common.Utils.Theme.ColorUtils exposing
    ( contrastColor
    , shiftColor
    , tintColor
    , tintOrShadeColor
    )

import Color exposing (Color)
import Color.Accessibility as Color
import Color.Manipulate as Color


colorContrastLight : Color
colorContrastLight =
    Color.rgb255 255 255 255


colorContrastDark : Color
colorContrastDark =
    Color.rgb255 0 0 0


tintOrShadeColor : Color -> Color -> Float -> Float -> Color
tintOrShadeColor colorContrast color tintAmount shadeAmount =
    if isColorContrastLight colorContrast then
        shadeColor color shadeAmount

    else
        tintColor color tintAmount


shiftColor : Color -> Float -> Color
shiftColor color weight =
    if weight > 0 then
        shadeColor color weight

    else
        tintColor color -weight


shadeColor : Color -> Float -> Color
shadeColor =
    Color.weightedMix colorContrastDark


tintColor : Color -> Float -> Color
tintColor =
    Color.weightedMix colorContrastLight


contrastColor : Color -> Color
contrastColor background =
    if Color.contrastRatio background colorContrastLight > 3 then
        colorContrastLight

    else
        colorContrastDark


isColorContrastLight : Color -> Bool
isColorContrastLight =
    (==) colorContrastLight
