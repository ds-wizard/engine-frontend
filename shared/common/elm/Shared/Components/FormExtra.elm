module Shared.Components.FormExtra exposing
    ( blockAfter
    , inlineSelect
    , mdAfter
    , text
    , textAfter
    )

import Form exposing (Form)
import Form.Input as Input
import Html exposing (Html, p)
import Html.Attributes exposing (class, disabled, id)
import Shared.Utils.Form.FormError exposing (FormError)
import Shared.Utils.Markdown as Markdown


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


inlineSelect : List ( String, String ) -> Form FormError o -> String -> Bool -> Html Form.Msg
inlineSelect options form fieldName isDisabled =
    Input.selectInput
        options
        (Form.getFieldAsString fieldName form)
        [ class "form-select form-control-inline", id fieldName, disabled isDisabled ]
