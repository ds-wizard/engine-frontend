module Ui.Breadcrumbs exposing
    ( Crumb
    , Words
    , crumb
    , current
    , custom
    , toHtml
    , toHtmlWithWords
    , withTestId
    )

{-| Where the page sits in whatever contains it.

Projects live in folders and knowledge models live in a library, so a listing is
no longer the top of anything and the title alone stops being enough to say
where you are.

An ordered list inside a `navigation` landmark named "Breadcrumb", which is the
APG pattern — the order is the point, so `ol` rather than `ul`. The last crumb
is the page itself: `aria-current="page"` and not a link, because a link to
where you already are is a link that does nothing.

A crumb is usually a link or the page itself; `custom` lets the caller put
something else in the trail, which is how a trail too long to draw shows its
middle as a menu.

The separator is a pseudo-element with an empty alternative text (`content: '/'
/ ''`), so a screen reader reads "Projects, Folder 1" rather than "Projects
slash Folder 1".

-}

import Html exposing (Html, a, li, nav, ol, span, text)
import Html.Attributes exposing (attribute, class, href)
import Ui.TestId as TestId


type Crumb msg
    = Crumb { kind : Kind msg, testId : Maybe String }


type Kind msg
    = Link { label : String, url : String }
    | Current String
    | Custom (Html msg)


crumb : { label : String, url : String } -> Crumb msg
crumb config =
    Crumb { kind = Link config, testId = Nothing }


{-| The page itself. Always last, and never a link.
-}
current : String -> Crumb msg
current label =
    Crumb { kind = Current label, testId = Nothing }


{-| A crumb that is neither a link nor the page: the middle of a trail too long
to draw, as a menu holding what was left out. The trail is a list of places, so
what stands in for several of them belongs in the list rather than beside it.
-}
custom : Html msg -> Crumb msg
custom content =
    Crumb { kind = Custom content, testId = Nothing }


withTestId : String -> Crumb msg -> Crumb msg
withTestId testId (Crumb config) =
    Crumb { config | testId = Just testId }


toHtml : List (Crumb msg) -> Html msg
toHtml =
    toHtmlWithWords { breadcrumb = "Breadcrumb" }


{-| The name of the landmark. `toHtml` says it in English.
-}
type alias Words =
    { breadcrumb : String
    }


{-| `toHtml`, with the words given.
-}
toHtmlWithWords : Words -> List (Crumb msg) -> Html msg
toHtmlWithWords words crumbs =
    nav [ class "ui-breadcrumbs", attribute "aria-label" words.breadcrumb ]
        [ ol [ class "ui-breadcrumbs__list" ] (List.map viewCrumb crumbs) ]


viewCrumb : Crumb msg -> Html msg
viewCrumb (Crumb entry) =
    let
        item =
            li (class "ui-breadcrumbs__item" :: TestId.attributes entry.testId)
    in
    case entry.kind of
        Link config ->
            item [ a [ class "ui-breadcrumbs__link", href config.url ] [ text config.label ] ]

        Current label ->
            item
                [ span
                    [ class "ui-breadcrumbs__current"
                    , attribute "aria-current" "page"
                    ]
                    [ text label ]
                ]

        Custom content ->
            item [ content ]
