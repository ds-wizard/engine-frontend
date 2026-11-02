module Ui.Page.Loading exposing (toHtml)

{-| The region between a route and its data.

A status region with the label in it from the start, and a spinner that waits.
The two halves are timed differently on purpose. The words are in the document
at once, so assistive technology has them however quickly the data arrives. The
picture fades in only after a short delay, so a page that loads in a moment goes
straight from nothing to its content instead of flashing a spinner on the way —
a flash that says "loading" after the fact is noise, and repeated on every
navigation it makes a fast application feel slow.

The delay is an animation on opacity and nothing else: no timer in Elm, no
message, and nothing about the layout moves when it ends. The global
reduced-motion rule removes the fade and keeps the delay.

The label is visible as well as announced, never a spinner alone. A spinner
says that something is happening; only the words say what, and a wait that is
long enough to see is long enough to want to know.

One caveat about announcing it. Some screen readers only speak changes to a live
region that already existed, so a region inserted with its text may not be read
at all. That is acceptable here because the route change has already moved focus
to the page and announced it; the status is the second word, not the only one.
A loading state inside a page that stays put belongs in a region that is always
present — `Ui.AsyncBlock`, or `Ui.Page.List`'s status.

-}

import Html exposing (Html, div, span, text)
import Html.Attributes exposing (attribute, class)
import Ui.Icon as Icon


toHtml : String -> Html msg
toHtml label =
    div
        [ class "ui-page-loading"
        , attribute "role" "status"
        , attribute "aria-live" "polite"
        ]
        [ div [ class "ui-page-loading__inner" ]
            [ span [ class "ui-page-loading__icon", attribute "aria-hidden" "true" ]
                [ Icon.toHtml Icon.spinner ]
            , span [] [ text label ]
            ]
        ]
