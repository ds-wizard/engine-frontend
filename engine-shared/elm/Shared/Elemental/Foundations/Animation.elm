module Shared.Elemental.Foundations.Animation exposing (..)

import Css exposing (Style, animationDelay, animationDuration, animationName, ms, num, property, sec)
import Css.Animations exposing (keyframes, opacity)
import Html.Styled as Html
import Html.Styled.Attributes exposing (css)


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
