module Registry.Components.Flash exposing
    ( error
    , success
    )

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Shared.Html exposing (emptyNode)


error : String -> Html msg
error =
    flashView "alert-danger"


success : String -> Html msg
success =
    flashView "alert-success"


flashView : String -> String -> Html msg
flashView className msg =
    if msg /= "" then
        div [ class ("alert " ++ className) ]
            [ text msg
            ]

    else
        emptyNode
