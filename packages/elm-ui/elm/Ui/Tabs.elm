module Ui.Tabs exposing
    ( Tab
    , focusTab
    , focusTabAt
    , panel
    , tab
    , tabElementId
    , toHtml
    , toHtmlWithTabIds
    , withBadge
    , withIcon
    )

{-| In-page tabs: one tab stop, arrows move between them.

**This is not the same component as `Ui.NavTabs`, even though they look
identical.** These switch a panel within the page and follow the APG tablist
pattern — `role="tablist"`, `aria-selected`, `aria-controls`, and a roving
tabindex so the group is a single Tab stop. `Ui.NavTabs` changes the route and
is a list of links.

The existing code uses the same markup for both and gives every tab
`tabindex 0`, which makes a five-tab bar five tab stops and tells assistive
technology nothing about the relationship to the panel.

Activation is automatic: an arrow key both moves focus and selects. That is the
right choice when panels are cheap to render, which ours are.


## Moving focus with the selection

The arrow keys send the new tab's `onSelect`, and the re-render moves the roving
`tabindex` — but not focus, which only a command can move. Without it focus stays
on the tab that was selected before, now `tabindex -1`, and Space selects that
one again. So the caller, which is the only one with an `update`, finishes the
job: when the tab's message arrives, return `focusTab`.

    TabSelected source ->
        ( { model | source = source }
        , Tabs.focusTab NoOp "import-source" source
        )

with the tabs rendered by `toHtmlWithTabIds identity` so that each button's id
is `tabElementId "import-source" source`. `toHtml` numbers the buttons instead,
and `focusTabAt` takes the number. Focusing the tab that was clicked is harmless
— it already has focus — so there is no need to tell a click from a key. Do not
return it for a selection the reader did not make, such as restoring the tab
from the URL: that would pull focus away from wherever they are.

`Browser.Dom.focus` waits for the next render, so the element it looks for is the
one the new selection produced.

-}

import Browser.Dom as Dom
import Html exposing (Html, button, div, li, span, text, ul)
import Html.Attributes exposing (attribute, class, classList, id, tabindex, type_)
import Html.Events exposing (onClick, preventDefaultOn)
import Json.Decode as Decode
import List.Extra as List
import Maybe.Extra as Maybe
import Task
import Ui.Icon as Icon exposing (Icon)


type Tab id msg
    = Tab
        { id : id
        , label : String
        , onSelect : msg
        , icon : Maybe Icon
        , badge : Maybe (Html msg)
        }


tab : { id : id, label : String, onSelect : msg } -> Tab id msg
tab config =
    Tab
        { id = config.id
        , label = config.label
        , onSelect = config.onSelect
        , icon = Nothing
        , badge = Nothing
        }


withIcon : Icon -> Tab id msg -> Tab id msg
withIcon icon (Tab config) =
    Tab { config | icon = Just icon }


withBadge : Html msg -> Tab id msg -> Tab id msg
withBadge badge (Tab config) =
    Tab { config | badge = Just badge }


toHtml :
    { id : String
    , label : String
    , active : id
    , tabs : List (Tab id msg)
    , full : Bool
    }
    -> Html msg
toHtml config =
    view (\index _ -> String.fromInt index) config


{-| `toHtml`, with each tab button's id made from its tab's own id rather than
its position: `tabElementId config.id (toKey tab.id)`. Use it whenever the
selection is known by id in `update`, which is where `focusTab` is returned.
-}
toHtmlWithTabIds :
    (id -> String)
    ->
        { id : String
        , label : String
        , active : id
        , tabs : List (Tab id msg)
        , full : Bool
        }
    -> Html msg
toHtmlWithTabIds toKey config =
    view (\_ tabId -> toKey tabId) config


{-| The id of a tab button: the tablist's id, then `-tab-`, then the tab's key —
its position from zero with `toHtml`, the key `toHtmlWithTabIds` made for it
otherwise.
-}
tabElementId : String -> String -> String
tabElementId tablistId key =
    tablistId ++ "-tab-" ++ key


{-| Move focus to a tab button, by the tablist's id and the tab's key. Return it
from `update` when a tab's `onSelect` arrives; see the module documentation.
-}
focusTab : msg -> String -> String -> Cmd msg
focusTab noop tablistId key =
    Task.attempt (always noop) (Dom.focus (tabElementId tablistId key))


{-| `focusTab` for tabs rendered by `toHtml`, where the key is the position.
-}
focusTabAt : msg -> String -> Int -> Cmd msg
focusTabAt noop tablistId index =
    focusTab noop tablistId (String.fromInt index)


view :
    (Int -> id -> String)
    ->
        { id : String
        , label : String
        , active : id
        , tabs : List (Tab id msg)
        , full : Bool
        }
    -> Html msg
view toKey config =
    let
        ids =
            List.map (\(Tab t) -> t.id) config.tabs

        activeIndex =
            ids
                |> List.indexedMap Tuple.pair
                |> List.find (\( _, candidate ) -> candidate == config.active)
                |> Maybe.map Tuple.first
                |> Maybe.withDefault 0

        onSelectAt index =
            config.tabs
                |> List.getAt (modBy (List.length config.tabs) index)
                |> Maybe.map (\(Tab t) -> t.onSelect)
    in
    div [ class "ui-tabs" ]
        [ ul
            [ classList
                [ ( "ui-tabs__list", True )
                , ( "ui-tabs__list--full", config.full )
                ]
            , attribute "role" "tablist"
            , attribute "aria-label" config.label
            , onArrowKeys onSelectAt activeIndex (List.length config.tabs)
            ]
            (List.indexedMap (\index (Tab entry) -> viewTab config (tabElementId config.id (toKey index entry.id)) (Tab entry)) config.tabs)
        ]


{-| The panel the tabs control: `role="tabpanel"`, with the id the tabs'
`aria-controls` already points at, named by the active tab's label.

Pass the same `id` as the tabs and the active tab's `label`. Render it right
after the tabs and swap its content in place — a panel is one element whose
contents change, so `aria-controls` never points at nothing.

There is no `tabindex` on it. The APG adds one only for a panel with nothing
focusable in it, so that Tab from the tablist lands somewhere; a panel holding a
form or a button already has a first stop, and a second one in front of it is a
press that does nothing.

-}
panel : { id : String, label : String } -> List (Html msg) -> Html msg
panel config content =
    div
        [ id (config.id ++ "-panel")
        , attribute "role" "tabpanel"
        , attribute "aria-label" config.label
        ]
        content


viewTab :
    { a | id : String, active : id }
    -> String
    -> Tab id msg
    -> Html msg
viewTab config elementId (Tab entry) =
    let
        isActive =
            entry.id == config.active
    in
    li [ attribute "role" "none", class "ui-tabs__item" ]
        [ button
            [ type_ "button"
            , id elementId
            , attribute "role" "tab"
            , attribute "aria-selected"
                (if isActive then
                    "true"

                 else
                    "false"
                )
            , attribute "aria-controls" (config.id ++ "-panel")

            -- Roving tabindex: the selected tab is the group's only tab stop.
            , tabindex
                (if isActive then
                    0

                 else
                    -1
                )
            , classList [ ( "ui-tabs__tab", True ), ( "is-active", isActive ) ]
            , onClick entry.onSelect
            ]
            (Maybe.values
                [ Maybe.map Icon.toHtml entry.icon
                , Just (label entry.label)
                , entry.badge
                ]
            )
        ]


{-| The label is wrapped so a hidden bold copy can reserve its own width.

Without it the tab resizes when it becomes active and the whole bar shifts. The
existing code solves it the same way, and it is worth keeping.

-}
label : String -> Html msg
label value =
    span [ class "ui-tabs__label", attribute "data-label" value ] [ text value ]


onArrowKeys : (Int -> Maybe msg) -> Int -> Int -> Html.Attribute msg
onArrowKeys onSelectAt activeIndex count =
    preventDefaultOn "keydown"
        (Decode.field "key" Decode.string
            |> Decode.andThen
                (\key ->
                    let
                        target =
                            case key of
                                "ArrowRight" ->
                                    Just (activeIndex + 1)

                                "ArrowLeft" ->
                                    Just (activeIndex - 1 + count)

                                "Home" ->
                                    Just 0

                                "End" ->
                                    Just (count - 1)

                                _ ->
                                    Nothing
                    in
                    case Maybe.andThen onSelectAt target of
                        Just msg ->
                            Decode.succeed ( msg, True )

                        Nothing ->
                            Decode.fail "not a navigation key"
                )
        )
