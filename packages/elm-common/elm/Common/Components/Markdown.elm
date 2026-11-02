module Common.Components.Markdown exposing (inline, view)

{-| Markdown someone else wrote — a readme, an announcement, a news item —
rendered into `Ui.Prose`.

**Headings are demoted.** A document's own `h1` is not the page's: the page has
one, and it is the template's title. `headingOffset` moves every heading down
the outline — with an offset of 1, a document's `h1` becomes an `h2` — so a
readme shows up in a screen reader's list of headings as a part of the page it
is on, not as a page of its own. Levels stop at `h6`, where HTML does; below it
the outline flattens rather than breaks. Pass the level of the heading the
document sits under: 1 directly under the page title, 2 inside a section with
an `h2`.

**Links to other sites open in a new tab**, as they always have, with
`rel="noopener noreferrer"` so the opened page cannot reach back through
`window.opener`. A link within the application — a path or a fragment — stays
in the tab, because a readme pointing at another page of the same application
is navigation, and a new tab for it would be a change of context nobody asked
for.

**Raw HTML is not rendered.** The parser is given no HTML renderers, so a
document with a tag in it is reported as a parse error rather than injected,
which is what the existing renderer does. `javascript:` URLs never reach the
document either: the virtual DOM refuses them.

-}

import Html exposing (Html, text)
import Html.Attributes as Attributes
import Html.Lazy as Lazy
import Markdown.Block as Block
import Markdown.Parser as Parser
import Markdown.Renderer as Renderer exposing (Renderer)
import Ui.Prose as Prose


view : { headingOffset : Int } -> String -> Html msg
view config =
    Lazy.lazy2 render config.headingOffset


render : Int -> String -> Html msg
render headingOffset markdown =
    let
        deadEndsToString deadEnds =
            deadEnds
                |> List.map Parser.deadEndToString
                |> String.join "\n"
    in
    case
        markdown
            |> Parser.parse
            |> Result.mapError deadEndsToString
            |> Result.andThen (Renderer.render (renderer headingOffset))
    of
        Ok rendered ->
            Prose.toHtml rendered

        Err errors ->
            Prose.toHtml [ Html.p [] [ text errors ] ]


{-| A sentence of markdown as inline content — a field's hint, where the
formatting is emphasis, a name in code or a link, and never a document.

Paragraphs become `span`s, so it sits inside the hint's own paragraph and takes
its size and color; a heading in a hint is a mistake, so it is set as one more
span. A link is a text link wherever the sentence is put, a hint or not.
Markdown that does not parse is shown as the text it was, because a hint
saying nothing is worse than a hint with an asterisk in it.

-}
inline : String -> List (Html msg)
inline markdown =
    case
        markdown
            |> Parser.parse
            |> Result.mapError (always ())
            |> Result.andThen (Result.mapError (always ()) << Renderer.render inlineRenderer)
    of
        Ok rendered ->
            rendered

        Err _ ->
            [ text markdown ]


inlineRenderer : Renderer (Html msg)
inlineRenderer =
    let
        defaultRenderer =
            Renderer.defaultHtmlRenderer
    in
    { defaultRenderer
        | paragraph = Html.span []
        , heading = \{ children } -> Html.span [] children
        , link =
            \link content ->
                Html.a (Attributes.class "c-markdown-link" :: Attributes.href link.destination :: titleAttributes link.title ++ targetAttributes link.destination) content
    }


renderer : Int -> Renderer (Html msg)
renderer headingOffset =
    let
        defaultRenderer =
            Renderer.defaultHtmlRenderer
    in
    { defaultRenderer
        | heading =
            \{ level, children } ->
                Html.node (headingTag headingOffset level) [] children
        , link =
            \link content ->
                Html.a (Attributes.href link.destination :: titleAttributes link.title ++ targetAttributes link.destination) content
    }


headingTag : Int -> Block.HeadingLevel -> String
headingTag offset level =
    "h" ++ String.fromInt (clamp 1 6 (Block.headingLevelToInt level + max 0 offset))


titleAttributes : Maybe String -> List (Html.Attribute msg)
titleAttributes title =
    case title of
        Just value ->
            [ Attributes.title value ]

        Nothing ->
            []


targetAttributes : String -> List (Html.Attribute msg)
targetAttributes destination =
    if isExternal destination then
        [ Attributes.target "_blank", Attributes.rel "noopener noreferrer" ]

    else
        []


isExternal : String -> Bool
isExternal destination =
    String.contains "://" destination || String.startsWith "//" destination
