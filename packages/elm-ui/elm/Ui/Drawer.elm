module Ui.Drawer exposing
    ( Drawer
    , Words
    , bodyId
    , drawer
    , focusTitle
    , titleId
    , toHtml
    , toHtmlWithWords
    , withFooter
    , withHeaderAction
    )

{-| A panel down the end of the application frame, beside the page rather than
over it: the AI assistant, and whatever else a reader keeps open while they
work.

Put it in `Ui.AppShell.withSidePanel` while it is open, and leave the slot
empty while it is closed: a closed drawer is not in the document, so it is not
a landmark, not a tab stop and not a column of the frame. The frame with no
drawer is exactly the frame without one.

**Not a dialog.** The page stays usable while it is open — that is the point of
a drawer over a modal — so nothing is inert, focus is not trapped, and it is
not `role="dialog"`. It is a complementary landmark (`aside`) named by its
title, so a screen reader lists it with the navigation and the main content and
can jump straight to it.

**Focus is the caller's to move, with two ids to move it to.** Opening it from a
control should move focus into it — to its first field (`focusTitle` when there
is none: the title takes focus programmatically) — and closing it should put
focus back on the control that opened it, which only the caller knows. Escape
anywhere inside it closes it, as the close button does.

**Narrow, it is over the page.** Below 48rem there is no room beside the
content, so the stylesheet lays it over the end of the page with a shadow;
nothing here has to know which.

-}

import Browser.Dom as Dom
import Html exposing (Html, aside, button, div, h2, text)
import Html.Attributes exposing (attribute, class, id, tabindex, type_)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Task
import Ui.Icon as Icon


type Drawer msg
    = Drawer
        { id : String
        , title : String
        , onClose : msg
        , content : List (Html msg)
        , footer : Maybe (Html msg)
        , headerAction : Maybe (Html msg)
        }


{-| What the drawer says of its own accord: the name of its close button.
English unless `toHtmlWithWords` says otherwise.
-}
type alias Words =
    { close : String
    }


english : Words
english =
    { close = "Close" }


{-| `id` is the drawer's, and must be unique on the page; the ids of its title
and its scrolling body are made from it (`titleId`, `bodyId`).
-}
drawer : { id : String, title : String, onClose : msg, content : List (Html msg) } -> Drawer msg
drawer config =
    Drawer
        { id = config.id
        , title = config.title
        , onClose = config.onClose
        , content = config.content
        , footer = Nothing
        , headerAction = Nothing
        }


{-| What stays under the scrolling body — a message box, the actions of a
form. It does not scroll with the content.
-}
withFooter : Html msg -> Drawer msg -> Drawer msg
withFooter footer (Drawer config) =
    Drawer { config | footer = Just footer }


{-| One control beside the title, before the close button — "New
conversation". It acts on everything in the drawer, so it belongs where the
drawer is named.
-}
withHeaderAction : Html msg -> Drawer msg -> Drawer msg
withHeaderAction action (Drawer config) =
    Drawer { config | headerAction = Just action }


titleId : String -> String
titleId drawerId =
    drawerId ++ "-title"


{-| The scrolling region, for keeping the newest of a conversation in sight
(`Browser.Dom.setViewportOf`).
-}
bodyId : String -> String
bodyId drawerId =
    drawerId ++ "-body"


{-| Move focus to the drawer's title, for a drawer with nothing to type into.
-}
focusTitle : msg -> String -> Cmd msg
focusTitle noop drawerId =
    Task.attempt (always noop) (Dom.focus (titleId drawerId))


toHtml : Drawer msg -> Html msg
toHtml =
    toHtmlWithWords english


toHtmlWithWords : Words -> Drawer msg -> Html msg
toHtmlWithWords words (Drawer config) =
    aside
        [ class "ui-drawer"
        , id config.id
        , attribute "aria-labelledby" (titleId config.id)
        , onEscape config.onClose
        ]
        [ div [ class "ui-drawer__header" ]
            [ -- An h2: the page's h1 is in the main region, and the drawer is
              -- a section of the screen beside it.
              h2 [ class "ui-drawer__title", id (titleId config.id), tabindex -1 ] [ text config.title ]
            , div [ class "ui-drawer__header-actions" ]
                [ Maybe.withDefault (text "") config.headerAction
                , button
                    [ type_ "button"
                    , class "ui-button ui-button--ghost ui-button--icon ui-button--sm"
                    , attribute "aria-label" words.close
                    , attribute "data-ui-tooltip" words.close
                    , attribute "data-ui-tooltip-placement" "end"
                    , onClick config.onClose
                    ]
                    [ Icon.toHtml (Icon.solid "fa-xmark") ]
                ]
            ]
        , div [ class "ui-drawer__body", id (bodyId config.id) ] config.content
        , case config.footer of
            Just footer ->
                div [ class "ui-drawer__footer" ] [ footer ]

            Nothing ->
                text ""
        ]


{-| Escape closes the drawer from anywhere inside it. Not stopped from going
further: a menu open inside the drawer closes on the same key, and so should
the drawer only once nothing inside has claimed it — a key a component has
handled does not arrive here.
-}
onEscape : msg -> Html.Attribute msg
onEscape msg =
    Html.Events.on "keydown"
        (Decode.field "key" Decode.string
            |> Decode.andThen
                (\key ->
                    if key == "Escape" then
                        Decode.succeed msg

                    else
                        Decode.fail "not Escape"
                )
        )
