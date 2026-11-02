module Ui.Prose exposing (toHtml)

{-| The typographic scope for content the system did not write — a rendered
README, an announcement, a news item. Headings, paragraphs, lists, quotes,
tables, code, rules, images and links inside it are styled from the roles and
the type scale; nothing outside it is touched.

    Prose.toHtml renderedMarkdown

**Demote the headings before they get here.** A document's own `h1` is not the
page's `h1`: a page has one, and it is the template's title. Two of them tell a
screen reader's heading list that the README is a page of its own. The scope
only changes how a heading _looks_ — a document's largest heading is a step
below the page title — and cannot change what level it _is_, because the level
is in the markup. The markdown wrapper in `Common.Components` shifts every
heading down under the page's own, and anything else rendering HTML into this
has to do the same.

A wide table scrolls sideways in its own box rather than pushing the page
(1.4.10). Markdown gives it no wrapper, so the table itself becomes the box; its
rows still lay out as a table. Some older versions of Safari drop a table's
semantics when it is not `display: table` — a renderer that can wrap its tables
in a scrolling `div` should.

-}

import Html exposing (Html, div)
import Html.Attributes exposing (class)


toHtml : List (Html msg) -> Html msg
toHtml content =
    div [ class "ui-prose" ] content
