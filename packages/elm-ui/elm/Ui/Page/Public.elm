module Ui.Page.Public exposing
    ( Public
    , bare
    , public
    , region
    , toHtml
    , withAside
    , withFooter
    , withIntro
    , withLinks
    )

{-| The unauthenticated shell: sign in, sign up, password reset, confirmations.

One of the few templates where the brand is allowed to be expressive — there is
no work in progress to compete with, and it is the first thing anyone sees.

Three shapes. `public` is the narrow panel with the page's title in it — a form
with a heading. `bare` is the same ground and the same brand with no panel and
no title, for a page that brings its own template: a knowledge model someone
opened without signing in is a detail page, not a form, and squeezing it into a
26rem card would be wrong twice — too narrow, and a second `h1` on top of the
one the detail page already has.

`region` is `bare` for an application page a visitor may open — a project shared
by its link. The header is a bar across the top, and below it the page is given
the rest of the window as `Ui.AppShell` gives it a region: full width, full
height, no padding, never scrolled as a whole (`ui-main`, the same container, so
the templates stack at the same widths). A detail page keeps its header while
its body scrolls, and a flush tab such as the questionnaire scrolls its own
regions, as for someone signed in.

-}

import Html exposing (Html, div, footer, header, li, main_, nav, section, ul)
import Html.Attributes exposing (attribute, class, classList, id, tabindex)
import Maybe.Extra as Maybe
import Ui.Button as Button
import Ui.Text as Text


type Public msg
    = Public
        { region : Bool
        , title : Maybe String
        , brand : Html msg
        , content : List (Html msg)
        , intro : Maybe (Html msg)
        , aside : Maybe (Html msg)
        , footer : Maybe (Html msg)
        , links : Maybe { label : String, links : List { label : String, url : String } }
        }


public : { title : String, brand : Html msg, content : List (Html msg) } -> Public msg
public config =
    Public
        { region = False
        , title = Just config.title
        , brand = config.brand
        , content = config.content
        , intro = Nothing
        , aside = Nothing
        , footer = Nothing
        , links = Nothing
        }


{-| No panel and no title: the content is a whole page template of its own and
carries the page's `h1`, which is where focus on a route change lands. The brand
moves into the header beside the links, because there is no panel for it to sit
above.
-}
bare : { brand : Html msg, content : List (Html msg) } -> Public msg
bare config =
    Public
        { region = False
        , title = Nothing
        , brand = config.brand
        , content = config.content
        , intro = Nothing
        , aside = Nothing
        , footer = Nothing
        , links = Nothing
        }


{-| The brand and the links in a bar across the top, and the rest of the window a
region the page fills, as it would in the application's shell. See the module's
documentation.
-}
region : { brand : Html msg, content : List (Html msg) } -> Public msg
region config =
    Public
        { region = True
        , title = Nothing
        , brand = config.brand
        , content = config.content
        , intro = Nothing
        , aside = Nothing
        , footer = Nothing
        , links = Nothing
        }


{-| Supporting text below the panel — the login screen's sidebar information.
-}
withAside : Html msg -> Public msg -> Public msg
withAside content (Public config) =
    Public { config | aside = Just content }


{-| Content that introduces the panel: what this instance is, before the form
that signs into it. The login screen's information text is the case.

Beside the panel when the page is wide enough for both, and between the brand and
the panel when it is not, which is also where it is in the document. The panel
keeps its width either way; the introduction takes the rest.

It is before the panel's `h1` in reading order, and focus on arrival still goes
to that heading: the form is what the page is for. Someone reading from the top
meets the introduction first, as it is on the screen.

-}
withIntro : Html msg -> Public msg -> Public msg
withIntro content (Public config) =
    Public { config | intro = Just content }


withFooter : Html msg -> Public msg -> Public msg
withFooter content (Public config) =
    Public { config | footer = Just content }


{-| The few places someone on a public page can go: "Log in", "Sign up", "Go to
the application".

A `nav` named by `label`, in a `header` before `main`. That makes it a landmark
a screen reader can list, and it puts the links first in the reading order —
which is where they are on the screen, at the top. They are link buttons rather
than text links, because they stand on their own rather than in a sentence and
want the hit target. A page with no links renders no header at all: an empty
navigation landmark is one more stop that leads nowhere.

-}
withLinks : { label : String, links : List { label : String, url : String } } -> Public msg -> Public msg
withLinks links (Public config) =
    Public { config | links = Just links }


toHtml : Public msg -> Html msg
toHtml (Public config) =
    let
        viewLinks =
            case config.links of
                Just { label, links } ->
                    if List.isEmpty links then
                        Nothing

                    else
                        Just
                            (nav [ class "ui-public__nav", attribute "aria-label" label ]
                                [ ul [ class "ui-public__links" ]
                                    (List.map
                                        (\link -> li [] [ Button.toHtml (Button.linkGhost link.label link.url) ])
                                        links
                                    )
                                ]
                            )

                Nothing ->
                    Nothing

        viewFooter =
            case config.footer of
                Just content ->
                    footer [ class "ui-public__footer" ] [ content ]

                Nothing ->
                    Html.text ""

        -- tabindex -1, so the skip target and the focus manager can land on it.
        viewMain children =
            main_ [ class "ui-public__main", id "ui-main", tabindex -1 ] children
    in
    case config.title of
        Just title ->
            div [ class "ui-public" ]
                [ case viewLinks of
                    Just links ->
                        header [ class "ui-public__header ui-public__header--end" ] [ links ]

                    Nothing ->
                        Html.text ""
                , viewMain
                    [ div
                        [ classList
                            [ ( "ui-public__inner", True )
                            , ( "ui-public__inner--intro", Maybe.isJust config.intro )
                            ]
                        ]
                        [ div [ class "ui-public__brand" ] [ config.brand ]
                        , case config.intro of
                            Just content ->
                                div [ class "ui-public__intro" ] [ content ]

                            Nothing ->
                                Html.text ""
                        , section [ class "ui-public__panel" ]
                            (Text.pageTitle title :: config.content)
                        , case config.aside of
                            Just content ->
                                div [ class "ui-public__aside" ] [ content ]

                            Nothing ->
                                Html.text ""
                        ]
                    ]
                , viewFooter
                ]

        Nothing ->
            if config.region then
                div [ class "ui-public ui-public--region" ]
                    [ header [ class "ui-public__header" ]
                        [ div [ class "ui-public__brand" ] [ config.brand ]
                        , Maybe.withDefault (Html.text "") viewLinks
                        ]
                    , main_ [ class "ui-public__region", id "ui-main", tabindex -1 ] config.content
                    ]

            else
                div [ class "ui-public ui-public--bare" ]
                    [ header [ class "ui-public__header" ]
                        [ div [ class "ui-public__brand" ] [ config.brand ]
                        , Maybe.withDefault (Html.text "") viewLinks
                        ]
                    , viewMain [ div [ class "ui-public__content" ] config.content ]
                    , viewFooter
                    ]
