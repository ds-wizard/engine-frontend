module Shared.Elemental.Foundations.Border exposing (default, roundedFull)

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
        , borderRadius (px2rem radiusDefault)
        ]


roundedFull : Style
roundedFull =
    Css.batch
        [ borderRadius (px 9999) ]
