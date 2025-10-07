module Common.Components.Container exposing (content, list, simpleForm)

import Html exposing (Html, div)
import Html.Attributes exposing (class)


simpleForm : List (Html msg) -> Html msg
simpleForm =
    div [ class "container container-max-sm my-3" ]


list : List (Html msg) -> Html msg
list =
    div [ class "container container-fluid my-3 px-4" ]


content : List (Html msg) -> Html msg
content =
    div [ class "container container-max-xxl container-fluid my-3" ]
