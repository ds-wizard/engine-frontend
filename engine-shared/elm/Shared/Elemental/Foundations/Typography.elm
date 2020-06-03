module Shared.Elemental.Foundations.Typography exposing
    ( copy1
    , copy1contrast
    , copy1danger
    , copy1light
    , copy1lighter
    , copy1link
    , copy1success
    , copy2
    , copy2contrast
    , copy2inversed
    , copy2light
    , copy2lighter
    , copy2link
    , heading1
    , heading2
    , heading2inversed
    , heading3
    , heading3inversed
    , navbarBrand
    )

import Css exposing (Color, FontSize, Rem, Style, bold, color, fontSize, fontWeight, important, normal)
import Shared.Elemental.Theme exposing (Theme)
import Shared.Elemental.Utils exposing (contrastRatio, px2rem)


sizeXL : Float
sizeXL =
    24


sizeLG : Float
sizeLG =
    18


sizeMD : Float
sizeMD =
    14


sizeSM : Float
sizeSM =
    11



-- Heading 1


heading1 : Theme -> Style
heading1 theme =
    Css.batch
        [ color theme.colors.textDefault
        , fontSize (px2rem sizeXL)
        , fontWeight bold
        ]



-- Heading 2


heading2 : Theme -> Style
heading2 theme =
    Css.batch
        [ color theme.colors.textDefault
        , fontSize (px2rem sizeLG)
        , fontWeight bold
        ]


heading2inversed : Theme -> Style
heading2inversed theme =
    Css.batch
        [ color theme.colors.textInversed
        , fontSize (px2rem sizeLG)
        , fontWeight bold
        ]


navbarBrand : Theme -> Style
navbarBrand theme =
    Css.batch
        [ important (color theme.colors.textDefault)
        , important (fontSize (px2rem sizeLG))
        , important (fontWeight bold)
        ]



-- Heading 3


heading3 : Theme -> Style
heading3 theme =
    Css.batch
        [ color theme.colors.textDefault
        , fontSize (px2rem sizeMD)
        , fontWeight bold
        ]


heading3inversed : Theme -> Style
heading3inversed theme =
    Css.batch
        [ color theme.colors.textInversed
        , fontSize (px2rem sizeMD)
        , fontWeight bold
        ]



-- Copy 1


copy1 : Theme -> Style
copy1 theme =
    Css.batch
        [ color theme.colors.textDefault
        , fontSize (px2rem sizeMD)
        , fontWeight normal
        ]


copy1link : Theme -> Style
copy1link theme =
    Css.batch
        [ color theme.colors.primary
        , fontSize (px2rem sizeMD)
        , fontWeight normal
        ]


copy1light : Theme -> Style
copy1light theme =
    Css.batch
        [ color theme.colors.textLight
        , fontSize (px2rem sizeMD)
        , fontWeight normal
        ]


copy1lighter : Theme -> Style
copy1lighter theme =
    Css.batch
        [ color theme.colors.textLighter
        , fontSize (px2rem sizeMD)
        , fontWeight normal
        ]


copy1contrast : Theme -> Color -> Style
copy1contrast =
    copyContrast sizeMD


copy1danger : Theme -> Style
copy1danger theme =
    Css.batch
        [ color theme.colors.danger
        , fontSize (px2rem sizeMD)
        , fontWeight normal
        ]


copy1success : Theme -> Style
copy1success theme =
    Css.batch
        [ color theme.colors.success
        , fontSize (px2rem sizeMD)
        , fontWeight normal
        ]



-- Copy 2


copy2 : Theme -> Style
copy2 theme =
    Css.batch
        [ color theme.colors.textDefault
        , fontSize (px2rem sizeSM)
        , fontWeight normal
        ]


copy2link : Theme -> Style
copy2link theme =
    Css.batch
        [ color theme.colors.primary
        , fontSize (px2rem sizeSM)
        , fontWeight normal
        ]


copy2light : Theme -> Style
copy2light theme =
    Css.batch
        [ color theme.colors.textLight
        , fontSize (px2rem sizeSM)
        , fontWeight normal
        ]


copy2lighter : Theme -> Style
copy2lighter theme =
    Css.batch
        [ color theme.colors.textLighter
        , fontSize (px2rem sizeSM)
        , fontWeight normal
        ]


copy2inversed : Theme -> Style
copy2inversed theme =
    Css.batch
        [ color theme.colors.textInversed
        , fontSize (px2rem sizeSM)
        ]


copy2contrast : Theme -> Color -> Style
copy2contrast =
    copyContrast sizeSM



-- Utils


copyContrast : Float -> Theme -> Color -> Style
copyContrast size theme backgroundColor =
    let
        defaultColorContrast =
            contrastRatio theme.colors.textDefault backgroundColor

        inversedColorContrast =
            contrastRatio theme.colors.textInversed backgroundColor

        textColor =
            if defaultColorContrast > inversedColorContrast then
                theme.colors.textDefault

            else
                theme.colors.textInversed
    in
    Css.batch
        [ color textColor
        , fontSize (px2rem size)
        ]
