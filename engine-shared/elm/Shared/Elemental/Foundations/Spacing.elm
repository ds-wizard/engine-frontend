module Shared.Elemental.Foundations.Spacing exposing (..)

import Css exposing (Style, marginBottom, marginLeft, marginRight, padding, padding2)
import Shared.Elemental.Utils exposing (px2rem)


xs : Float
xs =
    4


sm : Float
sm =
    8


md : Float
md =
    16


lg : Float
lg =
    32


xl : Float
xl =
    64


xxl : Float
xxl =
    128


gridComfortable : Float
gridComfortable =
    lg + sm


gridCozy : Float
gridCozy =
    md


gridCompact : Float
gridCompact =
    xs


insetXS : Style
insetXS =
    padding (px2rem xs)


insetSM : Style
insetSM =
    padding (px2rem sm)


insetMD : Style
insetMD =
    padding (px2rem md)


insetLG : Style
insetLG =
    padding (px2rem lg)


insetXL : Style
insetXL =
    padding (px2rem xl)


insetSquishXS : Style
insetSquishXS =
    padding2 (px2rem (xs / 2)) (px2rem xs)


insetSquishSM : Style
insetSquishSM =
    padding2 (px2rem xs) (px2rem sm)


insetSquishMD : Style
insetSquishMD =
    padding2 (px2rem sm) (px2rem md)


insetSquishLG : Style
insetSquishLG =
    padding2 (px2rem md) (px2rem lg)


insetStretchSM : Style
insetStretchSM =
    padding2 (px2rem (sm + xs)) (px2rem sm)


insetStretchMD : Style
insetStretchMD =
    padding2 (px2rem (md + sm)) (px2rem md)


insetStretchLG : Style
insetStretchLG =
    padding2 (px2rem (lg + md)) (px2rem lg)


inlineXS : Style
inlineXS =
    marginRight (px2rem xs)


inlineSM : Style
inlineSM =
    marginRight (px2rem sm)


inlineMD : Style
inlineMD =
    marginRight (px2rem md)


inlineLG : Style
inlineLG =
    marginRight (px2rem lg)


inlineXL : Style
inlineXL =
    marginRight (px2rem xl)


stackXS : Style
stackXS =
    marginBottom (px2rem xs)


stackSM : Style
stackSM =
    marginBottom (px2rem sm)


stackMD : Style
stackMD =
    marginBottom (px2rem md)


stackLG : Style
stackLG =
    marginBottom (px2rem lg)


stackXL : Style
stackXL =
    marginBottom (px2rem xl)
