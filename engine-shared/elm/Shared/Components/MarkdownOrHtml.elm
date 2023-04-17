module Shared.Components.MarkdownOrHtml exposing (view)

import Html exposing (Html, div)
import Html.Parser
import Html.Parser.Util
import Shared.Markdown as Markdown


view : List (Html.Attribute msg) -> String -> Html msg
view attributes str =
    let
        toMarkdown =
            Markdown.toHtml attributes
    in
    if detectHtml str then
        case Html.Parser.run str of
            Ok result ->
                div attributes (Html.Parser.Util.toVirtualDom result)

            Err _ ->
                toMarkdown str

    else
        toMarkdown str


detectHtml : String -> Bool
detectHtml str =
    String.startsWith "<" str
