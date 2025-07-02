module Wizard.Common.View.Flash exposing
    ( error
    , info
    , loader
    , success
    , warning
    , warningHtml
    )

import Gettext exposing (gettext)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Shared.Html exposing (emptyNode, faSet)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (dataCy)


error : AppState -> String -> Html msg
error appState =
    flashView "alert-danger" <| faSet "_global.error" appState


warning : AppState -> String -> Html msg
warning appState =
    flashView "alert-warning" <| faSet "_global.warning" appState


warningHtml : AppState -> Html msg -> Html msg
warningHtml appState content =
    flashViewHtml "alert-warning" (faSet "_global.warning" appState) content


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
        (gettext "Loading..." appState.locale)


flashView : String -> Html msg -> String -> Html msg
flashView className icon msg =
    if msg /= "" then
        div
            [ class ("alert " ++ className)
            , dataCy ("flash_" ++ className)
            ]
            [ icon
            , text msg
            ]

    else
        emptyNode


flashViewHtml : String -> Html msg -> Html msg -> Html msg
flashViewHtml className icon msg =
    div
        [ class ("d-flex align-items-baseline alert " ++ className)
        , dataCy ("flash_" ++ className)
        ]
        [ icon
        , msg
        ]
