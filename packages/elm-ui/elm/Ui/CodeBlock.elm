module Ui.CodeBlock exposing
    ( fill
    , languages
    , toHtml
    , withActions
    )

{-| Code being shown rather than written.

The same editor as `Ui.Field.code`, read-only. Read-only rather than not
editable: the text can still be reached, selected and copied with the keyboard,
which is the difference between showing code and showing a picture of it.

It carries a name, because a region of code with no name is announced as a text
box with nothing in front of it. Say what the code is — "Handler code", "The
response" — not "code".

-}

import Html exposing (Html, div)
import Html.Attributes as Attr exposing (attribute, class)
import Json.Encode as Encode


{-| The languages the editor knows, which is the union of what the two existing
ones know between them. Anything else is plain text — an unknown language should
look like code that is not highlighted, not like a failure.
-}
languages : List String
languages =
    [ "python", "css", "html", "xml", "jinja2", "json" ]


toHtml : { label : String, language : String, value : String } -> Html msg
toHtml config =
    view "ui-code-block" config []


{-| Something in the corner — a copy button, usually.

Passed in rather than built in: `Ui.Copy` carries state and a port, and a block
of code that could not be shown without wiring both would be a worse component
than one that takes whatever the caller already has.

-}
withActions : List (Html msg) -> { label : String, language : String, value : String } -> Html msg
withActions actions config =
    view "ui-code-block" config actions


{-| The block as the page's main content — a log, the body of a request: as
wide as the content and as tall as what is left of it, scrolling inside itself
instead of stopping at the usual height. For a page that is there to show it,
with `Ui.Page.Detail.withFillContent`; anywhere else it is a block of 15rem.
-}
fill : { label : String, language : String, value : String } -> Html msg
fill config =
    view "ui-code-block ui-code-block--fill" config []


view : String -> { label : String, language : String, value : String } -> List (Html msg) -> Html msg
view classes config actions =
    div [ class classes ]
        (Html.node "ui-code-editor"
            [ class "ui-code-editor ui-code-editor--static"
            , attribute "language" config.language
            , attribute "readonly" ""

            -- On the element, for the element to put on the editor's own
            -- content: an aria-label out here would name a wrapper nobody reads.
            , attribute "label" config.label
            , Attr.property "editorValue" (Encode.string config.value)
            ]
            []
            :: (if List.isEmpty actions then
                    []

                else
                    [ div [ class "ui-code-block__actions" ] actions ]
               )
        )
