module Ui.Theme.Palette exposing
    ( darkConstraints
    , darkSheet
    , lightConstraints
    , lightSheet
    , toHex
    )

{-| The static neutral ramps, mirrored from `_roles.scss`.

They exist in Elm as well as SCSS because `Check.violations` has to reason about
the whole resolved sheet, and the derivation needs to know which surface a role
must clear. The two must agree; `PaletteTest` is what keeps them honest.

-}

import Color exposing (Color)
import Ui.Theme.Check as Check
import Ui.Theme.Derive as Derive exposing (BrandRoles)
import Ui.Theme.Oklch as Oklch



-- LIGHT NEUTRALS — OKLCH hue 250, chroma 0.006


neutral0 : Color
neutral0 =
    Color.rgb255 0xFF 0xFF 0xFF


neutral50 : Color
neutral50 =
    Color.rgb255 0xF3 0xF6 0xFA


neutral100 : Color
neutral100 =
    Color.rgb255 0xEB 0xEF 0xF2


neutral500 : Color
neutral500 =
    Color.rgb255 0x81 0x84 0x87


neutral550 : Color
neutral550 =
    Color.rgb255 0x63 0x66 0x6A


neutral600 : Color
neutral600 =
    Color.rgb255 0x5B 0x5E 0x61


neutral900 : Color
neutral900 =
    Color.rgb255 0x22 0x24 0x27



-- DARK CHARCOAL — OKLCH hue 250, chroma 0.010


charcoal0 : Color
charcoal0 =
    Color.rgb255 0x07 0x0A 0x0E


charcoal25 : Color
charcoal25 =
    Color.rgb255 0x0E 0x12 0x16


charcoal50 : Color
charcoal50 =
    Color.rgb255 0x16 0x1A 0x1E


charcoal100 : Color
charcoal100 =
    Color.rgb255 0x1F 0x23 0x27


charcoal200 : Color
charcoal200 =
    Color.rgb255 0x2A 0x2E 0x33


charcoal300 : Color
charcoal300 =
    Color.rgb255 0x38 0x3C 0x41


charcoal500 : Color
charcoal500 =
    Color.rgb255 0x91 0x96 0x9B


charcoal600 : Color
charcoal600 =
    Color.rgb255 0xB3 0xB8 0xBE


charcoal900 : Color
charcoal900 =
    Color.rgb255 0xDC 0xE2 0xE8


lightConstraints : Derive.Constraints
lightConstraints =
    { textSurface = neutral100
    , textOnSubtle = neutral900
    , borderOnSubtle = neutral500
    , linkSurface = lightStateExtreme
    }


darkConstraints : Derive.Constraints
darkConstraints =
    { textSurface = charcoal200
    , textOnSubtle = charcoal900
    , borderOnSubtle = charcoal500
    , linkSurface = darkStateExtreme
    }



-- VALUE SCALE


{-| The three stops the value scale is interpolated through, per mode: red at
0, yellow at 0.5, green at 1, on the danger and success hues' side of the circle.

Light mode keeps the colors at the lightness that makes them read as red,
yellow and green, and that is too light for most of them to clear 3:1 against
the bar's track — a yellow that did would be olive. So light mode gives every
fill an edge instead (`lightValueEdge`), and the edge is what is checked. On the
dark track every step clears 3:1 on its own, and the edge is transparent.

-}
lightValueStops : { from : Oklch.Oklch, via : Oklch.Oklch, to : Oklch.Oklch }
lightValueStops =
    { from = { l = 0.6, c = 0.21, h = 27 }
    , via = { l = 0.84, c = 0.17, h = 88 }
    , to = { l = 0.6, c = 0.16, h = 150 }
    }


darkValueStops : { from : Oklch.Oklch, via : Oklch.Oklch, to : Oklch.Oklch }
darkValueStops =
    { from = { l = 0.66, c = 0.19, h = 27 }
    , via = { l = 0.82, c = 0.16, h = 88 }
    , to = { l = 0.7, c = 0.16, h = 150 }
    }


{-| `value-0` to `value-100`, as `_roles.scss` names them.
-}
valueScale : { from : Oklch.Oklch, via : Oklch.Oklch, to : Oklch.Oklch } -> List ( String, Color )
valueScale stops =
    List.indexedMap (\index color -> ( "value-" ++ String.fromInt (index * 10), color )) (Derive.valueScale stops)


{-| The control border: already verified at 3:1 on the sunken surface the track
is painted in.
-}
lightValueEdge : Color
lightValueEdge =
    neutral500



-- SHEETS


{-| The furthest a state layer moves a surface a control can sit on: the raised
surface pressed in light mode, the overlay surface pressed in dark. Nothing
hovers on the sunken surface — that is table headers, wells and disabled inputs.

Precomputed rather than blended here, so the value in the stylesheet and the
value under test cannot drift apart silently.

-}
lightStateExtreme : Color
lightStateExtreme =
    Color.rgb255 0xE6 0xE6 0xE6


darkStateExtreme : Color
darkStateExtreme =
    Color.rgb255 0x46 0x49 0x4E


lightSheet : BrandRoles -> Check.Sheet
lightSheet brand =
    { surface = neutral0
    , surfaceFrame = neutral50
    , surfaceSunken = neutral100
    , surfaceElevated1 = neutral0
    , surfaceElevated2 = neutral0
    , surfaceElevated3 = neutral0
    , surfaceStateExtreme = lightStateExtreme
    , text = neutral900
    , textMuted = neutral600
    , textSubtle = neutral550
    , textOnBrand = neutral0
    , link = brand.link
    , linkHover = brand.linkHover
    , borderControl = neutral500
    , focusRing = brand.link
    , brandStrong = brand.strong
    , brandHover = brand.hover
    , brandActive = brand.active
    , brandSubtle = brand.subtle
    , brandSubtleSelected = brand.subtleSelected
    , brandOnSubtle = brand.onSubtle
    , statuses =
        [ status "success" 0x30 0x86 0x43 0xDF 0xF8 0xE2 0x48 0x9C 0x58 0x0B 0x6C 0x2B neutral0
        , status "warning" 0x9E 0x6D 0x00 0xFF 0xEE 0xD4 0xB8 0x80 0x00 0x7E 0x56 0x00 neutral0
        , status "danger" 0xC9 0x4D 0x45 0xFF 0xEB 0xE8 0xE1 0x62 0x58 0xAB 0x30 0x2B neutral0
        , status "info" 0x00 0x7E 0xB0 0xE1 0xF3 0xFF 0x00 0x94 0xCE 0x00 0x64 0x8D neutral0
        , status "private" 0x6D 0x33 0xD6 0xF1 0xEA 0xFD 0x5B 0x21 0xB6 0x5B 0x21 0xB6 neutral0
        ]
    , chart =
        [ ( "chart-1", Color.rgb255 0x01 0x89 0xF1 )
        , ( "chart-2", Color.rgb255 0xAE 0x7F 0x07 )
        , ( "chart-3", Color.rgb255 0x4C 0x98 0x00 )
        , ( "chart-4", Color.rgb255 0xE0 0x20 0x20 )
        , ( "chart-5", Color.rgb255 0xB6 0x20 0xE0 )
        , ( "chart-6", Color.rgb255 0x06 0x98 0x7E )
        , ( "chart-7", Color.rgb255 0x62 0x36 0xFF )
        , ( "chart-8", Color.rgb255 0xE5 0x5B 0x03 )
        ]
    , syntax =
        [ ( "syntax-comment", neutral550 )
        , ( "syntax-keyword", Color.rgb255 0x7C 0x3A 0xED )
        , ( "syntax-string", Color.rgb255 0x0A 0x30 0x69 )
        , ( "syntax-number", Color.rgb255 0x05 0x50 0xAE )
        , ( "syntax-name", Color.rgb255 0x95 0x38 0x00 )
        ]
    , valueScale = valueScale lightValueStops
    , valueEdge = Just lightValueEdge
    }


darkSheet : BrandRoles -> Check.Sheet
darkSheet brand =
    { surface = charcoal50
    , surfaceFrame = charcoal25
    , surfaceSunken = charcoal25
    , surfaceElevated1 = charcoal100
    , surfaceElevated2 = charcoal200
    , surfaceElevated3 = charcoal300
    , surfaceStateExtreme = darkStateExtreme
    , text = charcoal900
    , textMuted = charcoal600
    , textSubtle = charcoal600
    , textOnBrand = charcoal0
    , link = brand.link
    , linkHover = brand.linkHover
    , borderControl = charcoal500
    , focusRing = brand.link
    , brandStrong = brand.strong
    , brandHover = brand.hover
    , brandActive = brand.active
    , brandSubtle = brand.subtle
    , brandSubtleSelected = brand.subtleSelected
    , brandOnSubtle = brand.onSubtle
    , statuses =
        [ status "success" 0x52 0xA6 0x61 0x1D 0x39 0x21 0x36 0x8C 0x49 0x75 0xCA 0x83 charcoal0
        , status "warning" 0xC4 0x89 0x00 0x42 0x2D 0x08 0xA1 0x70 0x00 0xEA 0xAD 0x41 charcoal0
        , status "danger" 0xED 0x6D 0x62 0x4E 0x22 0x1E 0xCB 0x4F 0x46 0xFF 0x9D 0x92 charcoal0
        , status "info" 0x10 0x9E 0xDA 0x0C 0x36 0x4B 0x00 0x84 0xB8 0x4B 0xC2 0xFF charcoal0
        , status "private" 0xA7 0x8B 0xFA 0x2B 0x1A 0x52 0x7C 0x5C 0xF0 0xC9 0xB6 0xFF charcoal0
        ]
    , chart =
        [ ( "chart-1", Color.rgb255 0x00 0x91 0xFF )
        , ( "chart-2", Color.rgb255 0xF7 0xB5 0x00 )
        , ( "chart-3", Color.rgb255 0x6D 0xD4 0x00 )
        , ( "chart-4", Color.rgb255 0xE0 0x20 0x20 )
        , ( "chart-5", Color.rgb255 0xB6 0x20 0xE0 )
        , ( "chart-6", Color.rgb255 0x44 0xD7 0xB6 )
        , ( "chart-7", Color.rgb255 0x6A 0x50 0xFF )
        , ( "chart-8", Color.rgb255 0xFA 0x64 0x00 )
        ]
    , syntax =
        [ ( "syntax-comment", Color.rgb255 0x91 0x98 0xA1 )
        , ( "syntax-keyword", Color.rgb255 0xFF 0x94 0x92 )
        , ( "syntax-string", Color.rgb255 0xA5 0xD6 0xFF )
        , ( "syntax-number", Color.rgb255 0x79 0xC0 0xFF )
        , ( "syntax-name", Color.rgb255 0xD2 0xA8 0xFF )
        ]
    , valueScale = valueScale darkValueStops
    , valueEdge = Nothing
    }


status :
    String
    -> Int
    -> Int
    -> Int
    -> Int
    -> Int
    -> Int
    -> Int
    -> Int
    -> Int
    -> Int
    -> Int
    -> Int
    -> Color
    -> Check.StatusGroup
status name br bg bb sr sg sb rr rg rb or_ og ob on =
    { name = name
    , base = Color.rgb255 br bg bb
    , subtle = Color.rgb255 sr sg sb
    , border = Color.rgb255 rr rg rb
    , onSubtle = Color.rgb255 or_ og ob
    , on = on
    }


toHex : Color -> String
toHex color =
    let
        rgba =
            Color.toRgba color

        digits =
            "0123456789abcdef"

        at index =
            String.slice index (index + 1) digits

        part value =
            let
                n =
                    round (value * 255)
            in
            at (n // 16) ++ at (modBy 16 n)
    in
    "#" ++ part rgba.red ++ part rgba.green ++ part rgba.blue
