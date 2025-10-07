module Common.Components.TypeHintInput.TypeHintInputItem exposing (complex, simple)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)


simple : (a -> String) -> a -> Html msg
simple toName item =
    div [ class "typehints-simple-item" ] [ text (toName item) ]


complex : List (Html msg) -> Html msg
complex =
    div [ class "typehints-complex-item" ]
