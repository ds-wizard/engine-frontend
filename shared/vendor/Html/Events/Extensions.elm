module Html.Events.Extensions exposing
    ( alwaysPreventDefaultOn
    , alwaysPreventDefaultOnWithDecoder
    )

import Html
import Html.Events exposing (preventDefaultOn, stopPropagationOn)
import Json.Decode as D


alwaysPreventDefaultOn : String -> msg -> Html.Attribute msg
alwaysPreventDefaultOn event msg =
    preventDefaultOn event (D.succeed ( msg, True ))


alwaysPreventDefaultOnWithDecoder : String -> D.Decoder msg -> Html.Attribute msg
alwaysPreventDefaultOnWithDecoder event decoder =
    preventDefaultOn event (D.map (\msg -> ( msg, True )) decoder)
