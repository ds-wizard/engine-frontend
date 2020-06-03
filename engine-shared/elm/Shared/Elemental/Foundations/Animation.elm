module Shared.Elemental.Foundations.Animation exposing
    ( delayed
    , fadeIn
    , fast
    , moveUp
    , slow
    )

import Css exposing (Style, animationDelay, animationDuration, animationName, ms, num, property, translateY)
import Css.Animations exposing (keyframes, opacity, transform)
import Html.Styled as Html
import Html.Styled.Attributes exposing (css)
import Shared.Elemental.Utils exposing (px2rem)


slow : Html.Attribute msg
slow =
    css [ animationDuration (ms 1000) ]


fast : Html.Attribute msg
fast =
    css [ animationDuration (ms 250) ]


delayed : Html.Attribute msg
delayed =
    css [ animationDelay (ms 400) ]


fadeIn : Html.Attribute msg
fadeIn =
    css
        [ property "animation-fill-mode" "forwards"
        , animationName
            (keyframes
                [ ( 0, [ opacity (num 0) ] )
                , ( 100, [ opacity (num 1) ] )
                ]
            )
        ]


moveUp : Html.Attribute msg
moveUp =
    css
        [ property "animation-fill-mode" "forwards"
        , animationName
            (keyframes
                [ ( 0, [ transform [ translateY (px2rem 20) ] ] )
                , ( 100, [ transform [ translateY (px2rem 0) ] ] )
                ]
            )
        ]
