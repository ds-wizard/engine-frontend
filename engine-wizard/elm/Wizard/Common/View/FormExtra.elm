module Wizard.Common.View.FormExtra exposing (blockAfter, mdAfter, text, textAfter)

import Html exposing (Html, p)
import Html.Attributes exposing (..)
import Markdown
import String


text : String -> Html msg
text str =
    p [ class "form-text text-muted" ] [ Html.text str ]


textAfter : String -> Html msg
textAfter str =
    blockAfter [ Html.text str ]


mdAfter : String -> Html msg
mdAfter str =
    blockAfter [ Markdown.toHtml [] str ]


blockAfter : List (Html msg) -> Html msg
blockAfter =
    p [ class "form-text form-text-after text-muted" ]
