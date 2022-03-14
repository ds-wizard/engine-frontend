module Shared.Markdown exposing (toHtml)

import Html exposing (Html, text)
import Html.Attributes as Attr
import Markdown.Parser as Markdown
import Markdown.Renderer


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
