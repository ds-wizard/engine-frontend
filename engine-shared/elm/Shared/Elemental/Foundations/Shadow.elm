module Shared.Elemental.Foundations.Shadow exposing
    ( colorDefault
    , colorPrimary
    , lg
    , md
    , outlinePrimary
    , sm
    , xl
    , xs
    )

import Css exposing (Color, Style, boxShadow4, zero)
import Shared.Elemental.Theme exposing (Theme)
import Shared.Elemental.Utils exposing (colorL40, px2rem)


colorDefault : Theme -> Color
colorDefault =
    .colors >> .shadow


colorPrimary : Theme -> Color
colorPrimary =
    .colors >> .primary >> colorL40


outlinePrimary : Theme -> Style
outlinePrimary =
    shadow 0 4 (.colors >> .primary)


xs : (Theme -> Color) -> Theme -> Style
xs =
    shadow 2 2


sm : (Theme -> Color) -> Theme -> Style
sm =
    shadow 2 4


md : (Theme -> Color) -> Theme -> Style
md =
    shadow 4 8


lg : (Theme -> Color) -> Theme -> Style
lg =
    shadow 4 16


xl : (Theme -> Color) -> Theme -> Style
xl =
    shadow 8 32


shadow : Float -> Float -> (Theme -> Color) -> Theme -> Style
shadow y b toColor theme =
    Css.batch
        [ boxShadow4 zero (px2rem y) (px2rem b) (toColor theme)
        ]
