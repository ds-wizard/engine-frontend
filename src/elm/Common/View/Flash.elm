module Common.View.Flash exposing
    ( error
    , info
    , loader
    , success
    , warning
    )

import Common.AppState exposing (AppState)
import Common.Html exposing (emptyNode, fa)
import Common.Locale exposing (l)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)


l_ : String -> AppState -> String
l_ =
    l "Common.View.Flash"


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


loader : AppState -> Html msg
loader appState =
    flashView "alert-inline-loader" "spinner fa-spin" <| l_ "loader.loading" appState


flashView : String -> String -> String -> Html msg
flashView className icon msg =
    if msg /= "" then
        div [ class ("alert " ++ className) ]
            [ fa icon
            , text msg
            ]

    else
        emptyNode
