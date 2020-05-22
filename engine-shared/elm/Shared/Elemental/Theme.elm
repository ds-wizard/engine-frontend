module Shared.Elemental.Theme exposing (Theme, default)

import Css exposing (Color, hex, rgba)


type alias Theme =
    { colors :
        { primary : Color
        , textDefault : Color
        , textLighter : Color
        , textLight : Color
        , textInversed : Color
        , background : Color
        , illustrations : Color
        , border : Color
        , shadow : Color
        , overlay : Color
        , success : Color
        , danger : Color
        }
    , logo :
        { url : String
        , width : Float
        , height : Float
        }
    }


default : Theme
default =
    { colors =
        { primary = hex "#F15A24"
        , textDefault = hex "#4D4948"
        , textLighter = hex "#9A9594"
        , textLight = hex "#CCC9C8"
        , textInversed = hex "#FFFFFF"
        , background = hex "#FFFFFF"
        , illustrations = hex "#F15A24"
        , border = rgba 0 0 0 0.1
        , shadow = rgba 0 0 0 0.07
        , overlay = rgba 0 0 0 0.5
        , success = hex "#28A745"
        , danger = hex "#E02020"
        }
    , logo =
        { url = "/img/logo.svg"
        , width = 35
        , height = 35
        }
    }
