module Common.Utils.MarkdownOrHtml exposing (toHtml)

import Common.Utils.Markdown as Markdown
import Html exposing (Html, div)
import Html.Parser
import Html.Parser.Util
import Regex exposing (Regex)


toHtml : List (Html.Attribute msg) -> String -> Html msg
toHtml attributes str =
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
detectHtml =
    Regex.contains htmlTagRegex


htmlTagRegex : Regex
htmlTagRegex =
    Maybe.withDefault Regex.never (Regex.fromString "<[^>]+>")
