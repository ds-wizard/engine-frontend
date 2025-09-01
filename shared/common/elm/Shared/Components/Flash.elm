module Shared.Components.Flash exposing
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
import Html.Attributes.Extensions exposing (dataCy)
import Html.Extra as Html
import Shared.Components.FontAwesome exposing (faError, faInfo, faSpinner, faSuccess, faWarning)


error : String -> Html msg
error =
    flashView "alert-danger" faError


warning : String -> Html msg
warning =
    flashView "alert-warning" faWarning


warningHtml : Html msg -> Html msg
warningHtml content =
    flashViewHtml "alert-warning" faWarning content


success : String -> Html msg
success =
    flashView "alert-success" faSuccess


info : String -> Html msg
info =
    flashView "alert-info" faInfo


loader : Gettext.Locale -> Html msg
loader locale =
    flashView "alert-inline-loader"
        faSpinner
        (gettext "Loading..." locale)


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
        Html.nothing


flashViewHtml : String -> Html msg -> Html msg -> Html msg
flashViewHtml className icon msg =
    div
        [ class ("d-flex align-items-baseline alert " ++ className)
        , dataCy ("flash_" ++ className)
        ]
        [ icon
        , msg
        ]
