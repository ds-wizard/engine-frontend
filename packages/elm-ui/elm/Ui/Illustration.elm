module Ui.Illustration exposing
    ( Illustration
    , download
    , empty
    , error
    , noResults
    , notAllowed
    , notFound
    , placeholder
    , success
    , toHtml
    )

{-| The drawings on a full-page message and an empty state.

**One per situation, not one per page.** Eight of them cover the application:
what a reader learns from the picture is which kind of moment this is — nothing
here yet, nothing found, something broke, you may not, it is coming, take the
file, it worked — and a ninth drawing for a ninth page would say nothing the
heading does not.

**The picture is a file, not code.** Each is an image the application supplies
under `assets/illustrations/`, named after the situation rather than after what
is drawn, so a different set can be dropped in without touching a line of Elm:
the wizard's and a customer's are the same eight names. This module
names the situations and draws the box; the application's stylesheet says which
file goes in each (`--ui-illustration-<name>`), and the format with it — the
wizard's are SVG, a set of PNGs is the same list with a different extension.

The box is 4:3 and the image is contained in it, so a set drawn to another shape
still fits and pages keep their height whichever set is installed. A raster set
wants 960×720, which covers the largest slot (18rem) at 3× density.

**Every illustration is decorative.** It sits beside a heading and a sentence
that already say everything the picture does — "Not Found", "The page you are
looking for was not found." — so it is a background image on an empty,
`aria-hidden` element: there is nothing for a screen reader to find and nothing
for Tab to land on. An `img` with an empty `alt` would do as well, but a
background is what lets the stylesheet choose the file.

**They leave the theme.** A drawing's colors are its own —
baked into the file, brand color and all — so they answer to no role and to no
contrast floor. That is why the accent is no longer `currentColor`: a set is
drawn for the application it belongs to.

-}

import Html exposing (Html, span)
import Html.Attributes exposing (attribute, class)


type Illustration
    = Illustration String


{-| Nothing here yet: a list, a panel or a tab with nothing in it.
-}
empty : Illustration
empty =
    Illustration "empty"


{-| A search or a set of filters that matched nothing. Distinct from `empty`:
there is something here, just not what was asked for.
-}
noResults : Illustration
noResults =
    Illustration "no-results"


{-| Something went wrong — a failed request, a lost connection, a server that is
misconfigured.
-}
error : Illustration
error =
    Illustration "error"


notFound : Illustration
notFound =
    Illustration "not-found"


notAllowed : Illustration
notAllowed =
    Illustration "not-allowed"


{-| Not here yet: a page still being built, a preview still being made, a thing
waiting to be set up.
-}
placeholder : Illustration
placeholder =
    Illustration "placeholder"


{-| The file is on its way, or is there to be taken.
-}
download : Illustration
download =
    Illustration "download"


{-| It worked, and there is nothing more to do: a confirmed address, a sent
message, a finished session.
-}
success : Illustration
success =
    Illustration "success"


toHtml : Illustration -> Html msg
toHtml (Illustration name) =
    span
        [ class ("ui-illustration ui-illustration--" ++ name)
        , attribute "aria-hidden" "true"
        ]
        []
