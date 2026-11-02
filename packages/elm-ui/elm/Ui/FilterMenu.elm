module Ui.FilterMenu exposing (toHeaderHtml, toHtml, toQuietHtml)

{-| One filter of a listing: a menu button whose label is the filter's value.

A filter with nothing chosen reads as its name — "Organization". Once something
is chosen the button reads as the choice — "DSW" — and turns from outlined to
filled. Either signal alone would be enough for somebody; together they are
enough for everybody. The fill is what a glance along a row of filters finds,
and the words are what is left for a reader who cannot tell the tint from the
outline (WCAG 1.4.1).

The value on its own would be ambiguous to a screen reader — "DSW, menu button"
says nothing about what DSW is — so while a value is shown, the accessible name
is "Organization: DSW". It starts with the name and contains the visible text,
which keeps speech input working: saying what is on the button still reaches it
(WCAG 2.5.3).

The menu opens with a header repeating the filter's name, because once a value
has replaced it on the button the list of choices is otherwise unlabeled. It is
start-aligned: when the filters wrap onto a row of their own on a narrow screen,
an end-aligned menu opens leftwards under the navigation. The sort, which is
always last on the toolbar, stays end-aligned. A filter that still ends up at
the end of a row opens towards the other edge instead (`Ui.Menu` turns a list
that would stick out of the window), so it never adds a scrollbar.

`value` and `active` are separate because they are not always the same thing. A
filter can hold a default that is not a restriction ("All versions"), and a
multi-select filter decides for itself how many of its values fit in the label
("Draft +2"). Pass a value whenever the filter is active — the active fill with
the filter's name in it is the one combination that would lean on color alone.

A table's column can carry its own filter in its header (`toHeaderHtml`): a
funnel beside the column's name, filled once the filter is on.

-}

import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class, classList)
import Ui.Icon as Icon
import Ui.Menu as Menu
import Ui.Tooltip as Tooltip


toHtml :
    { id : String
    , name : String
    , value : Maybe String
    , active : Bool
    , items : List (Menu.Item msg)
    , menu : Menu.Model
    , toMsg : Menu.Msg -> msg
    }
    -> Html msg
toHtml config =
    let
        ( shown, accessibleName ) =
            case config.value of
                Just value ->
                    ( value, config.name ++ ": " ++ value )

                Nothing ->
                    ( config.name, config.name )
    in
    div
        [ classList
            [ ( "ui-filter-menu", True )
            , ( "ui-filter-menu--active", config.active )
            ]
        ]
        [ Menu.toHtml
            { id = config.id
            , trigger =
                Menu.custom
                    { label = accessibleName
                    , class = "ui-button ui-button--secondary ui-filter-menu__trigger"
                    , content =
                        [ span [ class "ui-filter-menu__value" ] [ text shown ]
                        , Icon.toHtml (Icon.solid "fa-chevron-down")
                        ]
                    }
            , alignment = Menu.Start
            , items = Menu.header config.name :: config.items
            , toMsg = config.toMsg
            }
            config.menu
        ]


{-| `toHtml`, as a workspace toolbar's menu (`Menu.quiet`): small, no border
and no fill, among the toolbar's other quiet controls. On, it takes the same
tint, without the outline a secondary button has.
-}
toQuietHtml :
    { id : String
    , name : String
    , value : Maybe String
    , active : Bool
    , items : List (Menu.Item msg)
    , menu : Menu.Model
    , toMsg : Menu.Msg -> msg
    }
    -> Html msg
toQuietHtml config =
    let
        ( shown, accessibleName ) =
            case config.value of
                Just value ->
                    ( value, config.name ++ ": " ++ value )

                Nothing ->
                    ( config.name, config.name )
    in
    div
        [ classList
            [ ( "ui-filter-menu", True )
            , ( "ui-filter-menu--active", config.active )
            ]
        ]
        [ Menu.toHtml
            { id = config.id
            , trigger =
                Menu.custom
                    { label = accessibleName
                    , class = "ui-button ui-button--quiet ui-button--sm ui-filter-menu__trigger"
                    , content =
                        [ span [ class "ui-filter-menu__value" ] [ text shown ]
                        , Icon.toHtml (Icon.solid "fa-chevron-down")
                        ]
                    }
            , alignment = Menu.Start
            , items = Menu.header config.name :: config.items
            , toMsg = config.toMsg
            }
            config.menu
        ]


{-| One column's filter, in the column's header (`Ui.DataGrid.withHeaderActions`):
a small funnel button beside the column's name rather than a labeled button in
a row of filters.

The column's name is right beside it, so the funnel shows no words; they are in
its accessible name instead. With nothing chosen that is `label` — "Filter
Name", which the caller words, since this tier knows no language — and once
something is, "Name: MIT", as `toHtml` says it. The funnel turns from quiet to
filled while the filter is on, and the name says it too (WCAG 1.4.1).

The list opens end-aligned, under the funnel and towards the column's name, and
is laid over the table instead of inside it: a header is in the table's own
scrolling region, which would clip a list positioned in it.

-}
toHeaderHtml :
    { id : String
    , name : String
    , label : String
    , value : Maybe String
    , active : Bool
    , items : List (Menu.Item msg)
    , menu : Menu.Model
    , toMsg : Menu.Msg -> msg
    }
    -> Html msg
toHeaderHtml config =
    let
        accessibleName =
            case config.value of
                Just value ->
                    config.name ++ ": " ++ value

                Nothing ->
                    config.label
    in
    div
        [ classList
            [ ( "ui-filter-menu ui-filter-menu--header", True )
            , ( "ui-filter-menu--active", config.active )
            ]
        ]
        [ Menu.toHtml
            { id = config.id
            , trigger =
                Menu.custom
                    { label = accessibleName
                    , class = "ui-button ui-button--quiet ui-button--sm ui-button--icon ui-filter-menu__trigger"
                    , content = [ Icon.toHtml (Icon.solid "fa-filter") ]
                    }
                    |> Menu.withTriggerTooltipAt Tooltip.End
            , alignment = Menu.End
            , items = Menu.header config.name :: config.items
            , toMsg = config.toMsg
            }
            config.menu
        ]
