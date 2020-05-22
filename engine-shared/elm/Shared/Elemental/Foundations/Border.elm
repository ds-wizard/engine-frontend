module Shared.Elemental.Foundations.Border exposing (default, roundedDefault, roundedFull)

import Css exposing (Style, border3, borderRadius, px, solid)
import Shared.Elemental.Theme exposing (Theme)
import Shared.Elemental.Utils exposing (px2rem)


radiusDefault : Float
radiusDefault =
    10


default : Theme -> Style
default theme =
    Css.batch
        [ border3 (px 1) solid theme.colors.border
        , roundedDefault
        ]


roundedDefault : Style
roundedDefault =
    Css.batch
        [ borderRadius (px2rem radiusDefault) ]


roundedFull : Style
roundedFull =
    Css.batch
        [ borderRadius (px 9999) ]
