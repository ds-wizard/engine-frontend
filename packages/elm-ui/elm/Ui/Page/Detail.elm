module Ui.Page.Detail exposing
    ( Detail
    , detail
    , toHtml
    , withActions
    , withActionsLeading
    , withBadges
    , withFillContent
    , withFlushContent
    , withFooter
    , withMenu
    , withMenuJoined
    , withNav
    , withSidePanel
    , withSidePanelWords
    , withTitleAside
    , withTitleIcon
    )

{-| The detail page: a header carrying the title, its status and its actions,
optional navigation between sections, the content, and a side panel of metadata.

16 modules use `DetailPage` today and four more use `DetailNavigation`; between
them they cover this shape. The side panel is where the existing pages put
identifiers, versions, organizations and links — everything true of the thing
being looked at rather than part of what you read.

-}

import Html exposing (Html, aside, div, header, section, span)
import Html.Attributes exposing (attribute, class, classList, style)
import Ui.Badge as Badge exposing (Badge)
import Ui.Button as Button exposing (Button)
import Ui.ButtonGroup as ButtonGroup
import Ui.Layout as Layout
import Ui.Resizer as Resizer
import Ui.Separator as Separator
import Ui.Text as Text


type Detail msg
    = Detail
        { title : String
        , badges : List (Badge msg)
        , actions : List (Button msg)
        , actionsLeading : Maybe (Html msg)
        , menu : Maybe (Html msg)
        , menuJoined : Maybe String
        , nav : Maybe (Html msg)
        , content : List (Html msg)
        , sidePanel : Maybe (SidePanel msg)
        , resizerWords : Maybe Resizer.Words
        , titleIcon : Maybe (Html msg)
        , titleAside : Maybe (Html msg)
        , flush : Bool
        , fill : Bool
        , footer : Maybe (Html msg)
        }


type alias SidePanel msg =
    { resizer : Resizer.Model
    , toMsg : Resizer.Msg -> msg
    , content : List (Html msg)
    }


detail : { title : String, content : List (Html msg) } -> Detail msg
detail config =
    Detail
        { title = config.title
        , badges = []
        , actions = []
        , actionsLeading = Nothing
        , menu = Nothing
        , menuJoined = Nothing
        , nav = Nothing
        , content = config.content
        , sidePanel = Nothing
        , resizerWords = Nothing
        , titleIcon = Nothing
        , titleAside = Nothing
        , flush = False
        , fill = False
        , footer = Nothing
        }


{-| Something to see before the title — the user's avatar on a user's page, a
template's format icon. Typically a `Ui.ItemIcon`.

Decorative: it sits outside the `h1` and is hidden from assistive technology,
because the title already says whose page this is, and "image, Jana Nováková,
heading level 1, Jana Nováková" says it twice. Anything the picture means that
the title does not say belongs in a badge instead.

-}
withTitleIcon : Html msg -> Detail msg -> Detail msg
withTitleIcon icon (Detail config) =
    Detail { config | titleIcon = Just icon }


{-| Something to see after the title and its badges, on the same line — who is
in the project right now, when the project is saving.

Unlike the title icon this is not decorative: it goes on the end of the title's
line, where it is read after the name of the thing it is about, and it carries
its own name — a `Ui.AvatarGroup` is a labeled list. Nothing here is
interactive, and nothing that only lives here may be: the line belongs to the
`h1`, and a control in it is a tab stop between the heading and the page's
actions. Actions belong in `withActions` or `withMenu`.

-}
withTitleAside : Html msg -> Detail msg -> Detail msg
withTitleAside aside_ (Detail config) =
    Detail { config | titleAside = Just aside_ }


{-| Something to look at before the actions, parted from them by a vertical rule
— the people editing the thing right now, beside the Share button that brings
more in. Not a control: the actions are the controls.
-}
withActionsLeading : Html msg -> Detail msg -> Detail msg
withActionsLeading leading (Detail config) =
    Detail { config | actionsLeading = Just leading }


withBadges : List (Badge msg) -> Detail msg -> Detail msg
withBadges badges (Detail config) =
    Detail { config | badges = badges }


withActions : List (Button msg) -> Detail msg -> Detail msg
withActions actions (Detail config) =
    Detail { config | actions = actions }


{-| The overflow of the header's actions, after the buttons: typically a
`Ui.Menu` with the kebab trigger.

A slot rather than a list of items, because the menu carries state the page
owns — open or closed, which item has focus — and the template has no business
holding it.

-}
withMenu : Html msg -> Detail msg -> Detail msg
withMenu menu (Detail config) =
    Detail { config | menu = Just menu }


{-| Join the menu to the last action as one split button — Share, and beside it
a chevron for what else there is to do with sharing — rather than a kebab of its
own at the end of the header. For a menu that is more of that action and not the
page's overflow; give it a `Ui.Menu.split` trigger.

The string names the `Ui.ButtonGroup` the two are joined in: "Sharing". With no
actions the menu stands where it would have.

-}
withMenuJoined : String -> Detail msg -> Detail msg
withMenuJoined label (Detail config) =
    Detail { config | menuJoined = Just label }


{-| Typically `Ui.NavTabs` — these move between routes, not between panels.
-}
withNav : Html msg -> Detail msg -> Detail msg
withNav nav (Detail config) =
    Detail { config | nav = Just nav }


{-| The content is one surface, edge to edge: a document in a frame, the
questionnaire. Off by default.

The body loses its padding — in every layout, the stacked one included — stops
scrolling, and gives the content its whole height and width, from the line
under the tabs to the bottom of the viewport. The surface does its own
scrolling; a body that scrolled as well would put a second scrollbar beside
the first, and a wheel over the edge would move the wrong one.

The first thing in the content takes that height, and anything after it gets
the height it needs underneath — so a page that shows one thing gets all of it,
and an alert that turns up beside it is still seen. Something that needs room
around it, a message or a loader, is not a surface: leave the body padded for
those and turn this on only while the surface is what is shown. Nothing clips
an outward focus ring at the body's edge any more, so the surface keeps its
own rings inside itself.

Stacked, a side panel still follows the content, and the two scroll as one
under the header as they always do: the surface is as tall as the region, and
the panel is below it.

-}
withFlushContent : Detail msg -> Detail msg
withFlushContent (Detail config) =
    Detail { config | flush = True }


{-| The content takes the body's whole width and height, and keeps its padding:
for a page that is there to show one thing that is long — a log, the body of a
request (`Ui.CodeBlock.fill`). Off by default.

The measure that keeps prose readable is dropped, and the content is a column
as tall as the body, in which what fills (a `Ui.CodeBlock.fill`, or a
`Ui.Layout.stack` holding one) takes what the rest leaves and scrolls inside
itself. When the window is too short for even the filling thing's least height,
the body scrolls as it always does.

Unlike `withFlushContent` it is still a page with room round what it shows —
titles, an alert above the block — rather than one surface edge to edge.

-}
withFillContent : Detail msg -> Detail msg
withFillContent (Detail config) =
    Detail { config | fill = True }


{-| A band along the bottom of the content, below its scrolling region: the
place for `Ui.Form.unsavedBar` on a tab that edits the thing. Off by default.

It is `Ui.Page.SplitNav`'s footer, in the same place for the same reason. It is
outside the scroll region rather than positioned over it, so what it holds is at
the bottom of the screen by construction — nothing is positioned, nothing
reserves room for it, and nothing is told how wide the side panel is. It has no
surface of its own and takes the body's inline padding, so the bar lines up with
the content above it, and an empty footer takes no room at all.

It runs under the content only. Side by side, the panel keeps its full height
beside both; stacked, the content and the panel scroll together above the
footer, so the bar stays on the bottom edge while either is read. A flush tab's
surface ends where the footer starts.

-}
withFooter : Html msg -> Detail msg -> Detail msg
withFooter footer (Detail config) =
    Detail { config | footer = Just footer }


{-| The panel of facts about the thing, down the end edge of the page.

It has its own ground rather than the content's, because a column that shares
the page's surface reads as more of the page rather than as something beside it
— and it is resizable, because 20rem of metadata is a guess and the person
reading it knows better. `Ui.Resizer` is the same separator the navigation uses.

-}
withSidePanel :
    { resizer : Resizer.Model, toMsg : Resizer.Msg -> msg }
    -> List (Html msg)
    -> Detail msg
    -> Detail msg
withSidePanel resize content (Detail config) =
    Detail
        { config
            | sidePanel =
                Just { resizer = resize.resizer, toMsg = resize.toMsg, content = content }
        }


{-| What the side panel's separator says, in the language of the page. Without
it the name is the one in the resizer's `Config` and the value is said in
English. See `Ui.Resizer.viewWithWords`.
-}
withSidePanelWords : Resizer.Words -> Detail msg -> Detail msg
withSidePanelWords words (Detail config) =
    Detail { config | resizerWords = Just words }


toHtml : Detail msg -> Html msg
toHtml (Detail config) =
    section [ class "ui-detail" ]
        [ header [ class "ui-detail__header" ]
            [ -- Three slots rather than two wrapping groups, so the stylesheet
              -- can keep the menu at the end of the title's line when the page
              -- is narrow and move only the buttons down.
              div [ class "ui-detail__heading" ]
                [ div [ class "ui-detail__title" ]
                    [ Layout.inline
                        ((case config.titleIcon of
                            Just icon ->
                                [ span [ attribute "aria-hidden" "true" ] [ icon ] ]

                            Nothing ->
                                []
                         )
                            ++ Text.pageTitle config.title
                            :: List.map Badge.toHtml config.badges
                            ++ (case config.titleAside of
                                    Just aside_ ->
                                        [ aside_ ]

                                    Nothing ->
                                        []
                               )
                        )
                    ]
                , viewActions config
                , viewMenu config
                ]
            , Maybe.withDefault (Html.text "") config.nav
            ]
        , -- One wrapper round the two regions under the header. Side by side it
          -- is `display: contents` and not a box at all, so each region keeps
          -- its own scrolling; stacked, it becomes the one scroll container, so
          -- the content and then the panel scroll together under a header that
          -- stays put.
          div [ class "ui-detail__scroll" ]
            [ -- Only the body scrolls: the header carries the title and the
              -- actions, and losing them on the way down a long page is worse
              -- than the space they cost.
              div
                [ classList
                    [ ( "ui-detail__body", True )
                    , ( "ui-detail__body--flush", config.flush )
                    , ( "ui-detail__body--fill", config.fill )
                    ]
                ]
                [ div [ class "ui-detail__inner" ]
                    [ div [ class "ui-detail__content" ] config.content ]
                ]
            , case config.sidePanel of
                Just panel ->
                    -- A complementary landmark, so it can be skipped: it holds
                    -- facts about the thing, not the thing itself.
                    aside
                        [ class "ui-detail__side"
                        , style "--ui-detail-side-width"
                            (String.fromFloat (Resizer.widthPx panel.resizer) ++ "px")
                        ]
                        [ -- The scrolling happens inside, so the separator does
                          -- not scroll away with the metadata it sits beside.
                          div [ class "ui-detail__side-scroll" ] panel.content
                        , case config.resizerWords of
                            Just words ->
                                Resizer.viewWithWords words panel.toMsg panel.resizer

                            Nothing ->
                                Resizer.view panel.toMsg panel.resizer
                        ]

                Nothing ->
                    Html.text ""
            ]
        , case config.footer of
            Just footer ->
                -- Outside the scroll wrapper, in both layouts, so the bar is
                -- below everything that scrolls and never over the panel.
                div [ class "ui-detail__footer" ] [ footer ]

            Nothing ->
                Html.text ""
        ]


{-| The menu, when it is joined to an action and there is one to join it to.
-}
joinedMenu : { a | actions : List (Button msg), menu : Maybe (Html msg), menuJoined : Maybe String } -> Maybe ( String, Html msg )
joinedMenu config =
    case ( config.menuJoined, config.menu, config.actions ) of
        ( Just label, Just menu, _ :: _ ) ->
            Just ( label, menu )

        _ ->
            Nothing


viewActions : { a | actions : List (Button msg), actionsLeading : Maybe (Html msg), menu : Maybe (Html msg), menuJoined : Maybe String } -> Html msg
viewActions config =
    let
        buttons =
            case ( joinedMenu config, List.reverse config.actions ) of
                ( _, [] ) ->
                    []

                ( Just ( label, menu ), last :: before ) ->
                    List.map Button.toHtml (List.reverse before)
                        ++ [ ButtonGroup.toHtml { label = label, items = [ Button.toHtml last, menu ] } ]

                ( Nothing, _ ) ->
                    List.map Button.toHtml config.actions

        leading =
            case ( config.actionsLeading, buttons ) of
                ( Just content, [] ) ->
                    [ content ]

                ( Just content, _ ) ->
                    [ content, Separator.vertical ]

                ( Nothing, _ ) ->
                    []
    in
    case leading ++ buttons of
        [] ->
            Html.text ""

        row ->
            div [ class "ui-detail__actions" ] [ Layout.inline row ]


viewMenu : { a | actions : List (Button msg), menu : Maybe (Html msg), menuJoined : Maybe String } -> Html msg
viewMenu config =
    case ( joinedMenu config, config.menu ) of
        ( Nothing, Just menu ) ->
            div [ class "ui-detail__menu" ] [ menu ]

        _ ->
            Html.text ""
