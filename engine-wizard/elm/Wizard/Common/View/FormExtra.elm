module Wizard.Common.View.FormExtra exposing
    ( blockAfter
    , inlineSelect
    , mdAfter
    , text
    , textAfter
    )

import Form exposing (Form)
import Form.Input as Input
import Html exposing (Html, p)
import Html.Attributes exposing (class, id)
import Shared.Form.FormError exposing (FormError)
import Shared.Markdown as Markdown


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


inlineSelect : List ( String, String ) -> Form FormError o -> String -> Html Form.Msg
inlineSelect options form fieldName =
    Input.selectInput
        options
        (Form.getFieldAsString fieldName form)
        [ class "form-control form-control-inline", id fieldName ]
