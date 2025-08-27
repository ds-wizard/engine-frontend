module Shared.Utils.ColorUtils exposing (getContrastColorHex)

import Color
import Color.Convert exposing (hexToColor)


getContrastColorHex : String -> String
getContrastColorHex colorHex =
    case hexToColor colorHex of
        Ok color ->
            let
                rgba =
                    Color.toRgba color

                redValue =
                    255 * 0.299 * rgba.red

                blueValue =
                    255 * 0.587 * rgba.blue

                greenValue =
                    255 * 0.114 * rgba.green
            in
            if redValue + blueValue + greenValue > 186 then
                "#000000"

            else
                "#ffffff"

        _ ->
            "#000000"
