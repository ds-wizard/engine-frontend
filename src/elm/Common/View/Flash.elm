module Common.View.Flash exposing
    ( error
    , info
    , loader
    , success
    , warning
    )

import Common.Html exposing (emptyNode, fa)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)


error : String -> Html msg
error =
    flashView "alert-danger" "exclamation-triangle"


warning : String -> Html msg
warning =
    flashView "alert-warning" "exclamation-triangle"


success : String -> Html msg
success =
    flashView "alert-success" "check"


info : String -> Html msg
info =
    flashView "alert-info" "info-circle"


loader : Html msg
loader =
    flashView "alert-inline-loader" "spinner fa-spin" "Loading..."


flashView : String -> String -> String -> Html msg
flashView className icon msg =
    if msg /= "" then
        div [ class ("alert " ++ className) ]
            [ fa icon
            , text msg
            ]

    else
        emptyNode
