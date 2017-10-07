module Common.Html exposing (..)

import Html exposing (Attribute, Html, a, div, h2, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onWithOptions)
import Json.Decode as Decode
import Msgs exposing (Msg)


onLinkClick : msg -> Attribute msg
onLinkClick message =
    let
        options =
            { stopPropagation = False
            , preventDefault = True
            }
    in
    onWithOptions "click" options (Decode.succeed message)


pageHeader : String -> List (Html Msg) -> Html Msg
pageHeader title actions =
    div [ class "header" ]
        [ h2 [] [ text title ]
        , pageActions actions
        ]


pageActions : List (Html Msg) -> Html Msg
pageActions actions =
    div [ class "actions" ]
        actions
