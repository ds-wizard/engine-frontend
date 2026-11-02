module Ui.Page.SplitNav exposing
    ( Item
    , ItemWithTestId
    , item
    , toHtml
    , toHtmlWithTestIds
    )

{-| Left navigation and a scrollable content area — the settings shape.

The navigation column is 20rem. The template owns its scroll container, so
scrolling to the top never needs to know which application it is in.

**On a narrow page the menu stops being a column.** Stacked above the content, a
long settings menu takes the height and leaves the form a strip — so below the
point where both fit side by side, the same links go into a disclosure that names
the section you are in: "Settings: Profile". Closed, the form has the page; open,
the links are a native `details`, which is a button with an expanded state to
everything that reads one, with nothing written to make it so.

**A group is labeled only when there is something to tell it from.** The
label names one set of links among several ("System Settings", "Plugins"); a
menu whose links are all of a kind passes `Nothing` and shows the list alone,
because a heading repeating the navigation's name says nothing the title above
it has not.

**The page's `h1` is the section, not the menu.** `title` names the navigation
("Settings") — it is the `nav`'s `aria-label`, and shown above the links in the
subtitle style, but as text rather than a heading, since a heading there would
precede the page's `h1` — the body is expected to begin with
`Ui.Text.pageTitle` for the section being edited ("Look & Feel"), which is also
where focus lands after choosing a section.

Both are rendered and the stylesheet shows one. The hidden one is `display:
none`, so it is out of the accessibility tree as well as off the screen — there
are never two menus to a screen reader.

-}

import Html exposing (Html, a, details, div, li, nav, section, span, summary, text, ul)
import Html.Attributes exposing (attribute, class, classList, href)
import Html.Keyed as Keyed
import List.Extra as List
import Ui.Icon as Icon
import Ui.TestId as TestId


type alias Item =
    { label : String
    , url : String
    , active : Bool
    }


item : { label : String, url : String, active : Bool } -> Item
item =
    identity


toHtml :
    { title : String
    , groups : List ( Maybe String, List Item )
    , body : List (Html msg)
    , footer : Maybe (Html msg)
    }
    -> Html msg
toHtml config =
    toHtmlWithTestIds
        { title = config.title
        , groups =
            List.map
                (Tuple.mapSecond (List.map (\entry -> { label = entry.label, url = entry.url, active = entry.active, testId = Nothing })))
                config.groups
        , body = config.body
        , footer = config.footer
        }


{-| An `Item` with a `data-cy` for the end-to-end tests.
-}
type alias ItemWithTestId =
    { label : String
    , url : String
    , active : Bool
    , testId : Maybe String
    }


{-| `toHtml`, where an item can carry a `data-cy`.

The id is on the link in the menu beside the content only, not on its copy in
the narrow page's disclosure: both are rendered, one is hidden, and a test id
matching two links — one of them invisible — is one a test cannot click.

-}
toHtmlWithTestIds :
    { title : String
    , groups : List ( Maybe String, List ItemWithTestId )
    , body : List (Html msg)
    , footer : Maybe (Html msg)
    }
    -> Html msg
toHtmlWithTestIds config =
    section [ class "ui-split-nav" ]
        [ nav [ class "ui-split-nav__nav", attribute "aria-label" config.title ]
            -- Text, not a heading: it repeats the navigation's own name, and
            -- a heading here would come before the page's h1.
            (div [ class "ui-subtitle ui-split-nav__title" ] [ text config.title ]
                :: List.map (viewGroup True) config.groups
            )
        , -- The navigation stays put and only the content scrolls, which is
          -- what the two existing implementations both do.
          div [ class "ui-split-nav__main" ]
            [ viewCompact config
            , div [ class "ui-split-nav__content" ]
                [ div [ class "ui-split-nav__inner" ] config.body ]
            , case config.footer of
                Just footer ->
                    -- Outside the scroll region rather than positioned over it.
                    -- A bar that has to be told where the sidebar ends is a bar
                    -- that will be wrong the first time the sidebar changes.
                    div [ class "ui-split-nav__footer" ] [ footer ]

                Nothing ->
                    Html.text ""
            ]
        ]


{-| A group's label is optional: a menu of one group has nothing to tell apart,
and a label there would only repeat the navigation's own name over the only
list on the page.
-}
viewGroup : Bool -> ( Maybe String, List ItemWithTestId ) -> Html msg
viewGroup withTestIds ( label, items ) =
    div [ class "ui-split-nav__group" ]
        [ case label of
            Just groupLabel ->
                span [ class "ui-split-nav__group-label" ] [ text groupLabel ]

            Nothing ->
                text ""
        , ul [ class "ui-split-nav__list" ] (List.map (viewItem withTestIds) items)
        ]


viewItem : Bool -> ItemWithTestId -> Html msg
viewItem withTestIds entry =
    li []
        [ a
            (href entry.url
                :: classList
                    [ ( "ui-split-nav__link", True )
                    , ( "is-active", entry.active )
                    ]
                :: (case ( withTestIds, entry.testId ) of
                        ( True, Just testId ) ->
                            [ TestId.attribute testId ]

                        _ ->
                            []
                   )
            )
            [ text entry.label ]
        ]


{-| The menu as a disclosure, for a page too narrow for a column.

Keyed on the current section, so arriving at another one renders a new, closed
`details` rather than leaving the old one open over the page you just chose.

-}
viewCompact : { a | title : String, groups : List ( Maybe String, List ItemWithTestId ) } -> Html msg
viewCompact config =
    let
        current =
            config.groups
                |> List.concatMap Tuple.second
                |> List.find .active
                |> Maybe.map .label
                |> Maybe.withDefault config.title
    in
    Keyed.node "div"
        [ class "ui-split-nav__compact" ]
        [ ( current
          , details [ class "ui-split-nav__disclosure" ]
                [ summary [ class "ui-split-nav__summary" ]
                    [ -- The title is part of the name and not of the picture:
                      -- "Profile" on its own does not say what it is a section
                      -- of.
                      span [ class "ui-visually-hidden" ] [ text (config.title ++ ": ") ]
                    , span [ class "ui-split-nav__current" ] [ text current ]
                    , span [ class "ui-split-nav__chevron", attribute "aria-hidden" "true" ]
                        [ Icon.toHtml (Icon.solid "fa-chevron-down") ]
                    ]
                , nav [ class "ui-split-nav__compact-nav", attribute "aria-label" config.title ]
                    (List.map (viewGroup False) config.groups)
                ]
          )
        ]
