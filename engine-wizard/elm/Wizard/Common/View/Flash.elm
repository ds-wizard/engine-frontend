module Wizard.Common.View.Flash exposing
    ( error
    , info
    , loader
    , success
    , warning
    )

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html exposing (emptyNode, faSet)
import Wizard.Common.Locale exposing (l)


l_ : String -> AppState -> String
l_ =
    l "Wizard.Common.View.Flash"


error : AppState -> String -> Html msg
error appState =
    flashView "alert-danger" <| faSet "_global.error" appState


warning : AppState -> String -> Html msg
warning appState =
    flashView "alert-warning" <| faSet "_global.warning" appState


success : AppState -> String -> Html msg
success appState =
    flashView "alert-success" <| faSet "_global.success" appState


info : AppState -> String -> Html msg
info appState =
    flashView "alert-info" <| faSet "_global.info" appState


loader : AppState -> Html msg
loader appState =
    flashView "alert-inline-loader"
        (faSet "_global.spinner" appState)
        (l_ "loader.loading" appState)


flashView : String -> Html msg -> String -> Html msg
flashView className icon msg =
    if msg /= "" then
        div [ class ("alert " ++ className) ]
            [ icon
            , text msg
            ]

    else
        emptyNode
