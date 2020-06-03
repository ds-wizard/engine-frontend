module Shared.Elemental.Foundations.Icon exposing (file)

import Css exposing (Color)
import Html.Styled exposing (Html, fromUnstyled)
import Shared.Elemental.Utils exposing (colorL20)
import Svg exposing (..)
import Svg.Attributes exposing (..)


file : String -> Color -> Html msg
file name color =
    let
        darkerColor =
            color.value

        lighterColor =
            (colorL20 color).value
    in
    fromUnstyled <|
        svg [ width "49px", height "52px", viewBox "0 0 49 52", version "1.1" ] [ g [ stroke "none", strokeWidth "1", fill "none", fillRule "evenodd" ] [ g [ id "icon", fillRule "nonzero" ] [ Svg.path [ d "M28.757,0 L8,0 C6.343,0 5,1.343 5,3 L5,49 C5,50.657 6.343,52 8,52 L42,52 C43.657,52 45,50.657 45,49 L45,16.243 C45,15.447 44.683,14.683 44.121,14.122 L30.878,0.879 C30.316,0.317 29.553,0 28.757,0", fill lighterColor ] [], Svg.path [ d "M44.121,14.122 L30.878,0.879 C30.623,0.624 30.322,0.427 30,0.279 L30,12 C30,13.657 31.343,15 33,15 L44.721,15 C44.574,14.678 44.377,14.377 44.121,14.122 Z", id "Path", fill darkerColor ] [], Svg.path [ d "M5,23 L44,23 C46.761,23 49,25.239 49,28 L49,40 C49,42.761 46.761,45 44,45 L5,45 C2.239,45 0,42.761 0,40 L0,28 C0,25.239 2.239,23 5,23 Z", id "Path", fill darkerColor ] [] ], text_ [ id "docx", fontFamily "NotoSans-Bold, Noto Sans", fontSize "11", fontWeight "bold", fill "#FFFFFF", textAnchor "middle" ] [ tspan [ x "24.5", y "38" ] [ text name ] ] ] ]
