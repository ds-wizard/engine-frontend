module Ui.Page.Content exposing
    ( Content
    , content
    , form
    , full
    , list
    , narrow
    , toHtml
    , withActions
    , withBreadcrumbs
    , withDescription
    , withTestId
    , withinPage
    )

{-| The default page: a header and a body.

103 modules in this codebase call `Page.header` today, which makes this the most
common shape by a wide margin. A page fills slots rather than assembling divs,
so its DOM order — and therefore its focus order — is fixed by the template
rather than by whoever wrote the view.

-}

import Html exposing (Html, div, header, section)
import Html.Attributes exposing (class, classList)
import Ui.Button as Button exposing (Button)
import Ui.Layout as Layout
import Ui.TestId as TestId
import Ui.Text as Text


type Content msg
    = Content
        { title : String
        , breadcrumbs : Maybe (Html msg)
        , description : Maybe String
        , actions : List (Button msg)
        , body : List (Html msg)
        , width : Width
        , testId : Maybe String
        , within : Bool
        }


type Width
    = Default
    | Narrow
    | Form
    | List
    | Full


content : { title : String, body : List (Html msg) } -> Content msg
content config =
    Content
        { title = config.title
        , breadcrumbs = Nothing
        , description = Nothing
        , actions = []
        , body = config.body
        , width = Default
        , testId = Nothing
        , within = False
        }


{-| The page as a region of a page that already has a heading — a form on the
tab of a detail page.

The `h1` belongs to whatever contains this, so the title is rendered as the
section's `h2` (`Ui.Text.title`) and does not take the shell's focus on a route
change. It stays visible, unlike a listing's: "New Document" is not what the tab
above it says.

The scroll container and the padding are the containing template's, so this
takes neither; the measure (`narrow`, `full`) still applies.

-}
withinPage : Content msg -> Content msg
withinPage (Content config) =
    Content { config | within = True }


{-| Where the page sits in whatever contains it — `Ui.Breadcrumbs`, above the
title. Above rather than beside it: the trail is what you read on the way to the
title, not a caption under it.
-}
withBreadcrumbs : Html msg -> Content msg -> Content msg
withBreadcrumbs breadcrumbs (Content config) =
    Content { config | breadcrumbs = Just breadcrumbs }


withDescription : String -> Content msg -> Content msg
withDescription description (Content config) =
    Content { config | description = Just description }


withActions : List (Button msg) -> Content msg -> Content msg
withActions actions (Content config) =
    Content { config | actions = actions }


narrow : Content msg -> Content msg
narrow (Content config) =
    Content { config | width = Narrow }


{-| The measure of a settings form — the width the settings pages give their
forms, and the width `Ui.Form.unsavedBar` is drawn at. For a form that saves
through that bar somewhere else, a detail page's settings tab say, so the form
and the bar under it line up.
-}
form : Content msg -> Content msg
form (Content config) =
    Content { config | width = Form }


{-| The measure of a list page (`Ui.Page.List`), for a page that lists its
items in a table of its own, so it lines up with the other lists.
-}
list : Content msg -> Content msg
list (Content config) =
    Content { config | width = List }


full : Content msg -> Content msg
full (Content config) =
    Content { config | width = Full }


{-| A `data-cy` attribute on the page's outermost element, for the end-to-end
tests.
-}
withTestId : String -> Content msg -> Content msg
withTestId testId (Content config) =
    Content { config | testId = Just testId }


toHtml : Content msg -> Html msg
toHtml (Content config) =
    let
        testIdAttributes =
            TestId.attributes config.testId

        inner =
            div
                [ classList
                    [ ( "ui-page__inner", True )
                    , ( "ui-page__inner--narrow", config.width == Narrow )
                    , ( "ui-page__inner--form", config.width == Form )
                    , ( "ui-page__inner--list", config.width == List )
                    , ( "ui-page__inner--full", config.width == Full )
                    , ( "ui-page__inner--within", config.within )
                    ]
                ]
                [ header [ class "ui-page__header" ]
                    [ Maybe.withDefault (Html.text "") config.breadcrumbs
                    , Layout.inlineBetween
                        [ div [ class "ui-page__heading" ]
                            [ if config.within then
                                Text.title config.title

                              else
                                Text.pageTitle config.title
                            , case config.description of
                                Just description ->
                                    Text.muted description

                                Nothing ->
                                    Html.text ""
                            ]
                        , Layout.inline (List.map Button.toHtml config.actions)
                        ]
                    ]
                , div [ class "ui-page__body" ] config.body
                ]
    in
    if config.within then
        -- No `.ui-page`: the template around this owns the scrolling and the
        -- padding, and a second scroll container inside it traps the wheel.
        div testIdAttributes [ inner ]

    else
        -- The outer element scrolls and spans the region; the inner one is the
        -- measure. Centering the scroll container instead would put the scrollbar
        -- in the middle of the page.
        section (class "ui-page" :: testIdAttributes) [ inner ]
