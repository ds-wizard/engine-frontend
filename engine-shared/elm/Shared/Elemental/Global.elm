module Shared.Elemental.Global exposing (..)

import Css exposing (..)
import Css.Global exposing (descendants, selector, typeSelector)
import Shared.Elemental.Foundations.Spacing as Spacing
import Shared.Elemental.Foundations.Typography as Typography
import Shared.Elemental.Theme exposing (Theme)
import Shared.Elemental.Utils exposing (colorL10, px2rem)


styles : Theme -> Style
styles theme =
    Css.batch
        [ Typography.copy1 theme
        , descendants
            [ typeSelector "ul"
                [ Spacing.stackMD
                , paddingLeft (px2rem Spacing.md)
                , descendants
                    [ typeSelector "li"
                        [ Spacing.stackSM ]
                    ]
                ]
            , selector "a:not([class])"
                [ Typography.copy1link theme
                , cursor pointer
                , textDecoration none
                , hover
                    [ color (colorL10 theme.colors.primary)
                    ]
                ]
            ]
        ]
