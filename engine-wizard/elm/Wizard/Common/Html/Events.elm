module Wizard.Common.Html.Events exposing (alwaysPreventDefaultOn, alwaysStopPropagationOn, onLinkClick)

import Html exposing (Attribute)
import Html.Events exposing (custom, preventDefaultOn, stopPropagationOn)
import Json.Decode as D


onLinkClick : msg -> Attribute msg
onLinkClick message =
    let
        options =
            { message = message
            , stopPropagation = False
            , preventDefault = True
            }
    in
    custom "click" (D.succeed options)


alwaysPreventDefaultOn : String -> D.Decoder msg -> Html.Attribute msg
alwaysPreventDefaultOn event decoder =
    preventDefaultOn event (D.map hijack decoder)


alwaysStopPropagationOn : String -> D.Decoder msg -> Html.Attribute msg
alwaysStopPropagationOn event decoder =
    stopPropagationOn event (D.map hijack decoder)


hijack : msg -> ( msg, Bool )
hijack msg =
    ( msg, True )
