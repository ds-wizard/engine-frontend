module Shared.Elemental.Components.Loader exposing (blockLG, blockSM, page)

import Css exposing (..)
import Css.Global exposing (class, descendants)
import Html.Styled exposing (Html, div, p, text)
import Html.Styled.Attributes exposing (css)
import Shared.Elemental.Foundations.Animation as Animation
import Shared.Elemental.Foundations.Size as Size
import Shared.Elemental.Foundations.Spacing as Spacing
import Shared.Elemental.Foundations.Typography as Typography
import Shared.Elemental.Theme exposing (Theme)
import Shared.Elemental.Utils exposing (px2rem)
import Shared.Html.Styled exposing (fa)


page : Theme -> Html msg
page theme =
    loader theme
        (Css.batch [ height (calc (vh 100) minus (px2rem Size.navigationHeight)) ])
        [ fa "fas fa-spinner fa-spin"
        , p [] [ text "Loading..." ]
        ]


blockSM : Theme -> Html msg
blockSM theme =
    loader theme
        (Css.batch [ height (pct 100) ])
        [ fa "fas fa-spinner fa-spin" ]


blockLG : Theme -> Html msg
blockLG theme =
    loader theme
        (Css.batch
            [ height (pct 100)
            , margin2 (px2rem Spacing.xl) zero
            ]
        )
        [ fa "fas fa-spinner fa-spin"
        , p [] [ text "Loading..." ]
        ]


loader : Theme -> Style -> List (Html msg) -> Html msg
loader theme specificStyle content =
    let
        styles =
            [ Typography.copy1light theme
            , textAlign center
            , opacity zero
            , displayFlex
            , flexDirection column
            , justifyContent center
            , alignItems center
            , descendants
                [ class "fa"
                    [ Spacing.stackSM
                    , fontSize (rem 2)
                    ]
                ]
            , specificStyle
            ]
    in
    div
        [ css styles
        , Animation.fadeIn
        , Animation.slow
        , Animation.delayed
        ]
        content
