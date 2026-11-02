module Ui.Page.List exposing
    ( ListPage
    , list
    , toHtml
    , withActions
    , withBreadcrumbs
    , withDescription
    , withPagination
    , withStatus
    , withinPage
    )

{-| The listing page: a header, a toolbar, the items, and the pagination.

Twenty-two modules call `Listing.view` today, and each owns the arrangement of
these parts as well as their state. This owns only the arrangement. Searching,
filtering, the query string and the fetch are the composite's
(`Components.Listing`); what it fills in here are slots, so the order the parts
are read and tabbed through is decided once:

1.  the header — breadcrumbs, the title, the description, the page's actions
2.  the toolbar — search, filters, sort
3.  the status — how many results, off the screen
4.  the content — the list, or an empty state
5.  the pagination

The header is `Ui.Page.Content`'s, full width, with one difference: the title is
the page's `h1`, carrying `Ui.AppShell.mainHeadingId` and `tabindex -1`, so the
shell's focus on a route change lands on it and announces the page. A listing is
the page most often arrived at, so it is the one that most needs to get that
right.

The actions come before the toolbar because they are about the page ("Create",
"Import") and the toolbar is about the list. A keyboard user reaching "Create"
should not have to walk past every filter first.

-}

import Html exposing (Html, div, h1, header, section, text)
import Html.Attributes exposing (attribute, class, id, tabindex)
import Ui.AppShell as AppShell
import Ui.Button as Button exposing (Button)
import Ui.Layout as Layout
import Ui.Text as Text


{-| Named `ListPage` rather than after the module, because `List` is taken by
the list every Elm module already imports.
-}
type ListPage msg
    = ListPage
        { title : String
        , toolbar : Html msg
        , content : List (Html msg)
        , breadcrumbs : Maybe (Html msg)
        , description : Maybe String
        , actions : List (Button msg)
        , pagination : Maybe (Html msg)
        , status : String
        , within : Bool
        }


list : { title : String, toolbar : Html msg, content : List (Html msg) } -> ListPage msg
list config =
    ListPage
        { title = config.title
        , toolbar = config.toolbar
        , content = config.content
        , breadcrumbs = Nothing
        , description = Nothing
        , actions = []
        , pagination = Nothing
        , status = ""
        , within = False
        }


{-| The listing as a region of a page that already has a heading — the documents
tab of a project, say.

The page's `h1` belongs to whatever contains this, so the title becomes the
section's `h2` (`Ui.Text.title`) and does not carry `Ui.AppShell.mainHeadingId`
— the same heading a form or a report on the next tab has. It is shown: the
page's actions sit at the end of the title's line, and without a title they were
a button alone on a line of its own, answering nothing. The tab above names the
listing too, which is the price of every tab looking like the others.

The scroll container and the padding are the containing template's too, so
nothing here takes them. Everything else — the actions, the toolbar, the status
region, the rows and the pagination — is `toHtml`'s.

-}
withinPage : ListPage msg -> ListPage msg
withinPage (ListPage config) =
    ListPage { config | within = True }


{-| `Ui.Pagination`, below the content. Taken as rendered Html because the
current page is state the template does not hold.
-}
withPagination : Html msg -> ListPage msg -> ListPage msg
withPagination pagination (ListPage config) =
    ListPage { config | pagination = Just pagination }


withActions : List (Button msg) -> ListPage msg -> ListPage msg
withActions actions (ListPage config) =
    ListPage { config | actions = actions }


{-| `Ui.Breadcrumbs`, above the title.
-}
withBreadcrumbs : Html msg -> ListPage msg -> ListPage msg
withBreadcrumbs breadcrumbs (ListPage config) =
    ListPage { config | breadcrumbs = Just breadcrumbs }


withDescription : String -> ListPage msg -> ListPage msg
withDescription description (ListPage config) =
    ListPage { config | description = Just description }


{-| What the list now holds, in words — "Showing 1–20 of 134", "No results".

A sighted user sees the rows change when they type into the search or pick a
filter; a screen reader user hears nothing, because focus is still in the search
box and nothing about it changed. This is what tells them the search did
something.

It is a polite live region, visually hidden, and rendered even while it is
empty. A live region only announces changes to itself, so one inserted together
with its first message is often not read at all — the region has to be in the
document before the text that should be spoken arrives. Keep the words short
and update them when the results arrive, not on every keystroke.

-}
withStatus : String -> ListPage msg -> ListPage msg
withStatus status (ListPage config) =
    ListPage { config | status = status }


toHtml : ListPage msg -> Html msg
toHtml (ListPage config) =
    let
        title =
            if config.within then
                Text.title config.title

            else
                h1
                    [ id AppShell.mainHeadingId
                    , class "ui-page-title ui-page-list__title"

                    -- The target of the shell's focus on a route
                    -- change. Without it the call fails silently.
                    , tabindex -1
                    ]
                    [ text config.title ]

        inner =
            div
                [ class
                    (if config.within then
                        "ui-page__inner ui-page__inner--full ui-page__inner--within"

                     else
                        "ui-page__inner ui-page__inner--list"
                    )
                ]
                [ header [ class "ui-page__header" ]
                    [ Maybe.withDefault (text "") config.breadcrumbs
                    , Layout.inlineBetween
                        [ div [ class "ui-page__heading" ]
                            [ title
                            , case config.description of
                                Just description ->
                                    Text.muted description

                                Nothing ->
                                    text ""
                            ]
                        , if List.isEmpty config.actions then
                            text ""

                          else
                            Layout.inline (List.map Button.toHtml config.actions)
                        ]
                    ]
                , div [ class "ui-page-list__listing" ]
                    [ config.toolbar
                    , div
                        [ class "ui-visually-hidden"
                        , attribute "role" "status"
                        , attribute "aria-live" "polite"
                        , attribute "aria-atomic" "true"
                        ]
                        [ text config.status ]
                    , div [ class "ui-page-list__content" ] config.content
                    , Maybe.withDefault (text "") config.pagination
                    ]
                ]
    in
    if config.within then
        -- No `.ui-page`: the template around this owns the scrolling and the
        -- padding, and a second scroll container inside it traps the wheel.
        inner

    else
        section [ class "ui-page" ] [ inner ]
