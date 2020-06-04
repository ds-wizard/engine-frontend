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
            [ typeSelector "h1"
                [ Typography.heading1 theme
                , Spacing.stackMD
                ]
            , typeSelector "h2"
                [ Typography.heading2 theme
                , Spacing.stackMD
                ]
            , selector "p + h2"
                [ marginTop (px2rem Spacing.lg) ]
            , typeSelector "h3"
                [ Typography.heading3 theme
                , Spacing.stackSM
                ]
            , selector "p + h3"
                [ marginTop (px2rem Spacing.md) ]
            , typeSelector "p"
                [ Spacing.stackMD ]
            , typeSelector "ul"
                [ Spacing.stackMD
                , paddingLeft (px2rem Spacing.md)
                , descendants
                    [ typeSelector "li"
                        [ Spacing.stackXS ]
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
            , selector "::placeholder"
                [ Typography.copy1light theme
                ]
            ]
        ]
