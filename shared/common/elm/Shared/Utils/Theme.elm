port module Shared.Utils.Theme exposing
    ( Theme
    , setTheme
    , toStyleString
    )

import Basics as Int
import Color exposing (Color)
import Color.Manipulate as Color
import Shared.Utils.Theme.ColorUtils as ColorUtils


type alias Theme =
    { primaryColor : Color
    , illustrationsColor : Color
    }


setTheme : Theme -> Cmd msg
setTheme =
    setThemePort << toStyleString


port setThemePort : String -> Cmd msg


toStyleString : Theme -> String
toStyleString theme =
    let
        primary =
            theme.primaryColor

        contrastColor =
            ColorUtils.contrastColor primary

        tintOrShade tint shade =
            colorToCssRgb <| ColorUtils.tintOrShadeColor contrastColor primary tint shade

        properties =
            [ { name = "--bs-bg-primary-color"
              , value = colorToCssRgb contrastColor
              }
            , { name = "--bs-btn-primary-active-bg"
              , value = tintOrShade btnActiveBgTintAmount btnActiveBgShadeAmount
              }
            , { name = "--bs-btn-primary-color"
              , value = colorToCssRgb contrastColor
              }
            , { name = "--bs-btn-primary-active-color"
              , value = colorToCssRgb contrastColor
              }
            , { name = "--bs-btn-primary-disabled-color"
              , value = colorToCssRgb contrastColor
              }
            , { name = "--bs-btn-primary-hover-bg"
              , value = tintOrShade btnHoverBgTintAmount btnHoverBgShadeAmount
              }
            , { name = "--bs-btn-primary-hover-color"
              , value = colorToCssRgb contrastColor
              }
            , { name = "--bs-focus-ring-color"
              , value = colorToRgbString (Color.weightedMix contrastColor primary btnShadowTintAmount)
              }
            , { name = "--bs-input-focus-border-color"
              , value = colorToCssRgb (ColorUtils.tintColor primary 0.5)
              }
            , { name = "--bs-link-color"
              , value = colorToCssRgb primary
              }
            , { name = "--bs-link-color-rgb"
              , value = colorToRgbString primary
              }
            , { name = "--bs-link-hover-color"
              , value = colorToCssRgb (ColorUtils.shiftColor primary linkShadePercentage)
              }
            , { name = "--bs-link-hover-color-rgb"
              , value = colorToRgbString (ColorUtils.shiftColor primary linkShadePercentage)
              }
            , { name = "--bs-primary"
              , value = colorToCssRgb theme.primaryColor
              }
            , { name = "--bs-primary-bg"
              , value = colorToCssRgb (ColorUtils.tintColor primary 0.9)
              }
            , { name = "--bs-primary-bg2"
              , value = colorToCssRgb (ColorUtils.tintColor primary 0.8)
              }
            , { name = "--bs-primary-rgb"
              , value = colorToRgbString theme.primaryColor
              }
            , { name = "--illustrations-color"
              , value = colorToCssRgb theme.illustrationsColor
              }
            ]
    in
    String.concat <| List.map propertyToString properties


type alias Property =
    { name : String
    , value : String
    }


propertyToString : Property -> String
propertyToString property =
    property.name ++ ": " ++ property.value ++ ";"


colorToRgbString : Color -> String
colorToRgbString color =
    let
        parts =
            Color.toRgba color

        componentToString =
            String.fromInt << Int.round << (*) 255
    in
    componentToString parts.red ++ ", " ++ componentToString parts.green ++ ", " ++ componentToString parts.blue


colorToCssRgb : Color -> String
colorToCssRgb color =
    "rgb(" ++ colorToRgbString color ++ ")"


btnHoverBgShadeAmount : Float
btnHoverBgShadeAmount =
    0.15


btnHoverBgTintAmount : Float
btnHoverBgTintAmount =
    0.15


btnActiveBgShadeAmount : Float
btnActiveBgShadeAmount =
    0.2


btnActiveBgTintAmount : Float
btnActiveBgTintAmount =
    0.2


btnShadowTintAmount : Float
btnShadowTintAmount =
    0.15


linkShadePercentage : Float
linkShadePercentage =
    0.2
