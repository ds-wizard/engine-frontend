module Common.View.FormExtra exposing (blockAfter, text, textAfter)

import Html exposing (Html, p)
import Html.Attributes exposing (..)
import String


text : String -> Html msg
text str =
    p [ class "form-text text-muted" ] [ Html.text str ]


textAfter : String -> Html msg
textAfter str =
    p [ class "form-text form-text-after text-muted" ] [ Html.text str ]


blockAfter : List (Html msg) -> Html msg
blockAfter =
    p [ class "form-text form-text-after text-muted" ]
