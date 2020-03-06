module Registry.Common.View.Flash exposing
    ( error
    , info
    , success
    , warning
    )

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Shared.Html exposing (emptyNode)


error : String -> Html msg
error =
    flashView "alert-danger"


warning : String -> Html msg
warning =
    flashView "alert-warning"


success : String -> Html msg
success =
    flashView "alert-success"


info : String -> Html msg
info =
    flashView "alert-info"


flashView : String -> String -> Html msg
flashView className msg =
    if msg /= "" then
        div [ class ("alert " ++ className) ]
            [ text msg
            ]

    else
        emptyNode
