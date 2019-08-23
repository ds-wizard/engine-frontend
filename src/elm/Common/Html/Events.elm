module Common.Html.Events exposing (onLinkClick)

import Html exposing (Attribute)
import Html.Events exposing (custom)
import Json.Decode as Decode


onLinkClick : msg -> Attribute msg
onLinkClick message =
    let
        options =
            { message = message
            , stopPropagation = False
            , preventDefault = True
            }
    in
    custom "click" (Decode.succeed options)
