module Shared.Elemental.Foundations.Shadow exposing
    ( colorDarker
    , colorDefault
    , colorPrimary
    , lg
    , md
    , outlinePrimary
    , sm
    , xl
    , xs
    , xxl
    )

import Css exposing (Color, Style, boxShadow4, zero)
import Shared.Elemental.Theme exposing (Theme)
import Shared.Elemental.Utils exposing (px2rem)


colorDefault : Theme -> Color
colorDefault =
    .colors >> .shadowDefault


colorDarker : Theme -> Color
colorDarker =
    .colors >> .shadowDarker


colorPrimary : Theme -> Color
colorPrimary =
    .colors >> .primary


outlinePrimary : Theme -> Style
outlinePrimary =
    shadow 0 4 (.colors >> .primary)


xs : (a -> Color) -> a -> Style
xs =
    shadow 2 2


sm : (a -> Color) -> a -> Style
sm =
    shadow 2 4


md : (a -> Color) -> a -> Style
md =
    shadow 4 8


lg : (a -> Color) -> a -> Style
lg =
    shadow 4 16


xl : (a -> Color) -> a -> Style
xl =
    shadow 8 32


xxl : (a -> Color) -> a -> Style
xxl =
    shadow 16 64


shadow : Float -> Float -> (a -> Color) -> a -> Style
shadow y b toColor theme =
    Css.batch
        [ boxShadow4 zero (px2rem y) (px2rem b) (toColor theme)
        ]
