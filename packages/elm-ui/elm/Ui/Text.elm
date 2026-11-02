module Ui.Text exposing
    ( announcement
    , body
    , bodyHtml
    , bodySmall
    , code
    , display
    , error
    , label
    , muted
    , pageDisplay
    , pageTitle
    , subtitle
    , title
    , visuallyHidden
    , visuallyHiddenInline
    )

{-| Typographic roles.

Muted text is a _role_, not a lighter color: `--ui-text-muted` clears 4.5:1
against every surface. Recession comes from size, weight and position, never
from fading toward the background.

-}

import Html exposing (Html, div, h1, h2, h3, p, span, text)
import Html.Attributes exposing (attribute, class, id, tabindex)
import Ui.AppShell as AppShell


display : String -> Html msg
display content =
    h1 [ class "ui-display" ] [ text content ]


{-| The page's own heading: the one `h1`, a step above the `h2` of a section
inside it.

It carries `Ui.AppShell.mainHeadingId` and `tabindex -1`, because that is where
the shell puts focus after a route change — an element without the id is not
found and one without the tabindex cannot take focus, and either way the move
fails silently. Every page template renders its title with this or
`pageDisplay`; a page drawn without a template has to as well.

-}
pageTitle : String -> Html msg
pageTitle content =
    h1 [ class "ui-page-title", id AppShell.mainHeadingId, tabindex -1 ] [ text content ]


{-| `pageTitle` at the display size, for full-page messages.
-}
pageDisplay : String -> Html msg
pageDisplay content =
    h1 [ class "ui-display", id AppShell.mainHeadingId, tabindex -1 ] [ text content ]


title : String -> Html msg
title content =
    h2 [ class "ui-title" ] [ text content ]


subtitle : String -> Html msg
subtitle content =
    h3 [ class "ui-subtitle" ] [ text content ]


body : String -> Html msg
body content =
    p [ class "ui-body" ] [ text content ]


{-| A paragraph with markup in it — a sentence carrying a name in bold or an
identifier in code. The words are still a sentence, so they are one paragraph
rather than a row of pieces; keep it to phrasing.
-}
bodyHtml : List (Html msg) -> Html msg
bodyHtml =
    p [ class "ui-body" ]


bodySmall : String -> Html msg
bodySmall content =
    p [ class "ui-body-sm" ] [ text content ]


{-| What went wrong with one thing in a list or beside it, in a sentence: the
danger ink and the mark a field's error has, so it reads as an error without
the weight of an alert. Not announced by itself — what reports the outcome
announces it — and not focusable; a page-level failure is an `Ui.Alert`.
-}
error : String -> Html msg
error content =
    p [ class "ui-text-error" ]
        [ span [ class "ui-text-error__icon", attribute "aria-hidden" "true" ] [ text "!" ]
        , span [] [ text content ]
        ]


muted : String -> Html msg
muted content =
    span [ class "ui-body-sm ui-text--muted" ] [ text content ]


label : String -> Html msg
label content =
    span [ class "ui-label" ] [ text content ]


code : String -> Html msg
code content =
    span [ class "ui-code" ] [ text content ]


{-| Available to assistive technology, not to the eye.
-}
visuallyHidden : String -> Html msg
visuallyHidden content =
    div [ class "ui-visually-hidden" ] [ text content ]


{-| `visuallyHidden` as a `span`, for words inside a sentence, a link or a
button — anywhere a block element is not allowed.
-}
visuallyHiddenInline : String -> Html msg
visuallyHiddenInline content =
    span [ class "ui-visually-hidden" ] [ text content ]


{-| The result of something the page just did, said to a screen reader and to
nobody else: a link put on the clipboard from a menu that has since closed, a
row copied, a value generated.

Components that own their own confirmation say it themselves — `Ui.Copy` has
this region built in, and a listing announces what it loaded. This is for the
outcome of an action whose control is gone by the time there is anything to
say, where the alternative is silence.

It is always in the document and empty until there is something to say: a live
region added at the moment it gets content is one some screen readers never
announce. Pass `""` for nothing, and the words otherwise; the caller clears it
again so that a second press of the same control is announced as well.

-}
announcement : String -> Html msg
announcement content =
    div
        [ class "ui-visually-hidden"
        , attribute "role" "status"
        , attribute "aria-live" "polite"
        ]
        [ text content ]
