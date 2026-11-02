module Ui.Theme.Oklch exposing
    ( Oklch
    , contrast
    , fromColor
    , luminance
    , toColor
    , withLightness
    )

{-| sRGB <-> OKLCH with hue-preserving gamut mapping.

The gamut mapping is the point of this module. Requesting a color's own chroma
at a lower lightness usually asks for something outside sRGB, and clipping the
channels shifts the hue — about 7 degrees toward violet for our own brand seed.
Chroma yields; hue and lightness hold.

-}

import Color exposing (Color)


type alias Oklch =
    { l : Float
    , c : Float
    , h : Float
    }



-- sRGB <-> LINEAR


srgbToLinear : Float -> Float
srgbToLinear c =
    if c <= 0.04045 then
        c / 12.92

    else
        ((c + 0.055) / 1.055) ^ 2.4


linearToSrgb : Float -> Float
linearToSrgb c =
    let
        clamped =
            clamp 0 1 c
    in
    if clamped <= 0.0031308 then
        12.92 * clamped

    else
        1.055 * (clamped ^ (1 / 2.4)) - 0.055



-- CONVERSION


fromColor : Color -> Oklch
fromColor color =
    let
        rgba =
            Color.toRgba color

        r =
            srgbToLinear rgba.red

        g =
            srgbToLinear rgba.green

        b =
            srgbToLinear rgba.blue

        lCone =
            (0.4122214708 * r + 0.5363325363 * g + 0.0514459929 * b) ^ (1 / 3)

        mCone =
            (0.2119034982 * r + 0.6806995451 * g + 0.1073969566 * b) ^ (1 / 3)

        sCone =
            (0.0883024619 * r + 0.2817188376 * g + 0.6299787005 * b) ^ (1 / 3)

        lightness =
            0.2104542553 * lCone + 0.793617785 * mCone - 0.0040720468 * sCone

        aAxis =
            1.9779984951 * lCone - 2.428592205 * mCone + 0.4505937099 * sCone

        bAxis =
            0.0259040371 * lCone + 0.7827717662 * mCone - 0.808675766 * sCone
    in
    { l = lightness
    , c = sqrt (aAxis * aAxis + bAxis * bAxis)
    , h = radiansToDegrees (atan2 bAxis aAxis)
    }


{-| Linear RGB for a given OKLCH triple, before clamping. Used to test whether a
color is representable in sRGB at all.
-}
toLinearRgb : Oklch -> { r : Float, g : Float, b : Float }
toLinearRgb oklch =
    let
        hRad =
            degrees oklch.h

        aAxis =
            oklch.c * cos hRad

        bAxis =
            oklch.c * sin hRad

        lCone =
            (oklch.l + 0.3963377774 * aAxis + 0.2158037573 * bAxis) ^ 3

        mCone =
            (oklch.l - 0.1055613458 * aAxis - 0.0638541728 * bAxis) ^ 3

        sCone =
            (oklch.l - 0.0894841775 * aAxis - 1.291485548 * bAxis) ^ 3
    in
    { r = 4.0767416621 * lCone - 3.3077115913 * mCone + 0.2309699292 * sCone
    , g = -1.2684380046 * lCone + 2.6097574011 * mCone - 0.3413193965 * sCone
    , b = -0.0041960863 * lCone - 0.7034186147 * mCone + 1.707614701 * sCone
    }


inGamut : Oklch -> Bool
inGamut oklch =
    let
        linear =
            toLinearRgb oklch

        ok value =
            value >= -0.000001 && value <= 1.000001
    in
    ok linear.r && ok linear.g && ok linear.b


{-| Reduce chroma, holding lightness and hue, until the color is representable.
-}
gamutMap : Oklch -> Oklch
gamutMap oklch =
    if inGamut oklch then
        oklch

    else
        { oklch | c = searchChroma oklch 0 oklch.c 50 }


searchChroma : Oklch -> Float -> Float -> Int -> Float
searchChroma oklch low high steps =
    if steps <= 0 then
        low

    else
        let
            mid =
                (low + high) / 2
        in
        if inGamut { oklch | c = mid } then
            searchChroma oklch mid high (steps - 1)

        else
            searchChroma oklch low mid (steps - 1)


toColor : Oklch -> Color
toColor oklch =
    let
        mapped =
            gamutMap oklch

        linear =
            toLinearRgb mapped
    in
    Color.rgb (linearToSrgb linear.r) (linearToSrgb linear.g) (linearToSrgb linear.b)


{-| The same color at a different lightness, hue and chroma preserved as far as
sRGB allows.
-}
withLightness : Float -> Oklch -> Oklch
withLightness lightness oklch =
    gamutMap { oklch | l = lightness }



-- CONTRAST


luminance : Color -> Float
luminance color =
    let
        rgba =
            Color.toRgba color
    in
    0.2126
        * srgbToLinear rgba.red
        + 0.7152
        * srgbToLinear rgba.green
        + 0.0722
        * srgbToLinear rgba.blue


{-| WCAG contrast ratio. Order does not matter.
-}
contrast : Color -> Color -> Float
contrast a b =
    let
        la =
            luminance a

        lb =
            luminance b
    in
    (max la lb + 0.05) / (min la lb + 0.05)


radiansToDegrees : Float -> Float
radiansToDegrees rad =
    let
        deg =
            rad * 180 / pi
    in
    if deg < 0 then
        deg + 360

    else
        deg
