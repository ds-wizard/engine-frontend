module Ui.DescriptionList exposing
    ( Entry
    , entry
    , toHtml
    , toHtmlColumns
    , toHtmlColumnsWideTerms
    , toHtmlGrid
    , withHtml
    , withTargetId
    , withTestId
    )

{-| Label-and-value pairs: what a detail page's side panel is made of.

Stacked rather than side by side. Two columns look tidier in a mock-up and fall
apart in use — the label column is as wide as its longest label, which leaves
the values in a narrow gutter, and a value that wraps to three lines beside a
one-word label reads as a mistake. Stacking gives every value the full width of
the panel and puts the label directly above what it names, which is also the
shorter distance for the eye to travel. `toHtmlColumns` is for the exception:
short labels with short values, where none of that happens.

A real `dl`, so the association is in the markup rather than in the layout.

-}

import Html exposing (Html, dd, div, dl, dt, text)
import Html.Attributes exposing (class, id, tabindex)
import Ui.TestId as TestId


type Entry msg
    = Entry { term : String, description : List (Html msg), targetId : Maybe String, testId : Maybe String }


entry : { term : String, description : String } -> Entry msg
entry config =
    Entry { term = config.term, description = [ text config.description ], targetId = Nothing, testId = Nothing }


{-| For a value that is a link, a badge, or anything else that is not a string.
-}
withHtml : { term : String, description : List (Html msg) } -> Entry msg
withHtml config =
    Entry { term = config.term, description = config.description, targetId = Nothing, testId = Nothing }


{-| Make the entry somewhere a link can take you: `#id` names its term.

The term gets the `id` and `tabindex -1`, because following an in-page link
must move focus as well as the view — without it a keyboard user is left on the
link and the next Tab goes on from there, and a screen reader says nothing. An
element without `tabindex` cannot take programmatic focus, and the jump would
fail silently. The term rather than the description, so what is read first on
arrival is the name of the thing the link was named after.

In a `Browser.application` the application has to act on the fragment itself —
`Ui.AppShell.focusFragment`, as for the skip links.

-}
withTargetId : String -> Entry msg -> Entry msg
withTargetId targetId (Entry config) =
    Entry { config | targetId = Just targetId }


{-| The entry's test hook, on its value — what a test reads — with the term
beside it as the value's previous sibling.
-}
withTestId : String -> Entry msg -> Entry msg
withTestId testId (Entry config) =
    Entry { config | testId = Just testId }


toHtml : List (Entry msg) -> Html msg
toHtml entries =
    dl [ class "ui-description-list" ] (List.concatMap viewEntry entries)


{-| Each label beside its value, in a column a quarter of the width: a reference
sheet of short labels with short values — versions, build dates — where
stacking them doubles the height and leaves the right side of the panel empty.
The column is a fixed share rather than as wide as the longest label, so lists
one after another line up with each other.

Each pair is a row in a `div`, which a `dl` allows, so the row can be
highlighted under the pointer — on a line this wide the eye needs help getting
from a label to its value.

-}
toHtmlColumns : List (Entry msg) -> Html msg
toHtmlColumns =
    viewColumns "ui-description-list ui-description-list--columns"


{-| `toHtmlColumns` with the labels in two fifths of the width: names with
their versions, where the name is the long part.
-}
toHtmlColumnsWideTerms : List (Entry msg) -> Html msg
toHtmlColumnsWideTerms =
    viewColumns "ui-description-list ui-description-list--columns ui-description-list--wide-terms"


viewColumns : String -> List (Entry msg) -> Html msg
viewColumns classes entries =
    dl [ class classes ]
        (List.map (div [ class "ui-description-list__row" ] << viewEntry) entries)


{-| The pairs side by side, as many to a line as fit, each label still above
its value: short figures that belong together — a plan's projects, users and
storage — where a column of them would be mostly empty space. Each pair is
wrapped in a `div`, which a `dl` allows, so it moves to the next line whole.
-}
toHtmlGrid : List (Entry msg) -> Html msg
toHtmlGrid entries =
    dl [ class "ui-description-list ui-description-list--grid" ]
        (List.map (div [ class "ui-description-list__item" ] << viewEntry) entries)


viewEntry : Entry msg -> List (Html msg)
viewEntry (Entry config) =
    let
        targetAttributes =
            case config.targetId of
                Just targetId ->
                    [ id targetId, tabindex -1 ]

                Nothing ->
                    []
    in
    [ dt (class "ui-description-list__term" :: targetAttributes) [ text config.term ]
    , dd (class "ui-description-list__description" :: TestId.attributes config.testId) config.description
    ]
