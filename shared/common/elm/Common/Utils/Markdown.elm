module Common.Utils.Markdown exposing
    ( sanitizeHtml
    , toHtml
    , toString
    )

import Html exposing (Html, text)
import Html.Attributes as Attr
import Markdown.Block as Block
import Markdown.Html
import Markdown.Parser as Markdown
import Markdown.Renderer


sanitizeHtml : String -> String
sanitizeHtml html =
    html
        |> String.replace "<" "&lt;"


toHtml : List (Html.Attribute msg) -> String -> Html msg
toHtml attrs markdownInput =
    let
        deadEndsToString deadEnds =
            deadEnds
                |> List.map Markdown.deadEndToString
                |> String.join "\n"
    in
    case
        markdownInput
            |> Markdown.parse
            |> Result.mapError deadEndsToString
            |> Result.andThen (Markdown.Renderer.render renderer)
    of
        Ok rendered ->
            Html.div (Attr.class "Markdown" :: attrs) rendered

        Err errors ->
            Html.div (Attr.class "Markdown" :: attrs) [ text errors ]


toString : String -> String
toString markdownInput =
    let
        deadEndsToString deadEnds =
            deadEnds
                |> List.map Markdown.deadEndToString
                |> String.join "\n"
    in
    case
        markdownInput
            |> Markdown.parse
            |> Result.mapError deadEndsToString
            |> Result.andThen (Markdown.Renderer.render stringRenderer)
    of
        Ok rendered ->
            String.join "\n" rendered

        Err _ ->
            ""


renderer : Markdown.Renderer.Renderer (Html msg)
renderer =
    let
        defaultRenderer =
            Markdown.Renderer.defaultHtmlRenderer
    in
    { defaultRenderer
        | link =
            \link content ->
                case link.title of
                    Just title ->
                        Html.a
                            [ Attr.href link.destination
                            , Attr.title title
                            , Attr.target "_blank"
                            ]
                            content

                    Nothing ->
                        Html.a [ Attr.href link.destination, Attr.target "_blank" ] content
    }


stringRenderer : Markdown.Renderer.Renderer String
stringRenderer =
    { heading =
        \{ children } ->
            String.concat children
    , paragraph = String.concat
    , hardLineBreak = "\n"
    , blockQuote = String.concat
    , strong = String.concat
    , emphasis = String.concat
    , strikethrough = String.concat
    , codeSpan = identity
    , link =
        \link content ->
            case link.title of
                Just title ->
                    title

                Nothing ->
                    String.concat content
    , image = always ""
    , text = identity
    , unorderedList =
        \items ->
            String.join "\n"
                (items
                    |> List.map
                        (\item ->
                            case item of
                                Block.ListItem _ children ->
                                    String.concat children
                        )
                )
    , orderedList =
        \_ items ->
            String.join "\n"
                (items
                    |> List.map
                        (\itemBlocks ->
                            String.concat itemBlocks
                        )
                )
    , html = Markdown.Html.oneOf []
    , codeBlock =
        \{ body } -> body
    , thematicBreak = "\n"
    , table = String.concat
    , tableHeader = String.concat
    , tableBody = String.concat
    , tableRow = String.concat
    , tableHeaderCell =
        \_ ->
            String.concat
    , tableCell =
        \_ ->
            String.concat
    }
