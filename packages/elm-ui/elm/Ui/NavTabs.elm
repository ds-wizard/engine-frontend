module Ui.NavTabs exposing
    ( Item
    , item
    , toHtml
    , withBadge
    , withIcon
    , withTestId
    )

{-| Tabs that change the route.

Visually identical to `Ui.Tabs`, semantically not the same thing at all. These
are links inside a `nav`, and the current one is marked with
`aria-current="page"` — they are navigation, and the APG tablist pattern would
be wrong for them: there is no panel in this document for `aria-controls` to
point at, and a screen reader announcing "tab, 2 of 5" would be describing
something the user is about to leave.

The rule: if activating it changes the URL, it belongs here. If it swaps a panel
in place, it belongs in `Ui.Tabs`.

-}

import Html exposing (Html, a, li, nav, span, text, ul)
import Html.Attributes exposing (attribute, class, classList, href)
import Maybe.Extra as Maybe
import Ui.Icon as Icon exposing (Icon)
import Ui.TestId as TestId


type Item msg
    = Item
        { label : String
        , url : String
        , active : Bool
        , icon : Maybe Icon
        , badge : Maybe (Html msg)
        , testId : Maybe String
        }


item : { label : String, url : String, active : Bool } -> Item msg
item config =
    Item
        { label = config.label
        , url = config.url
        , active = config.active
        , icon = Nothing
        , badge = Nothing
        , testId = Nothing
        }


withIcon : Icon -> Item msg -> Item msg
withIcon icon (Item config) =
    Item { config | icon = Just icon }


withBadge : Html msg -> Item msg -> Item msg
withBadge badge (Item config) =
    Item { config | badge = Just badge }


{-| A `data-cy` on the tab's link, for the end-to-end tests.
-}
withTestId : String -> Item msg -> Item msg
withTestId testId (Item config) =
    Item { config | testId = Just testId }


toHtml : { label : String, items : List (Item msg), full : Bool } -> Html msg
toHtml config =
    nav [ attribute "aria-label" config.label, class "ui-tabs" ]
        [ ul
            [ classList
                [ ( "ui-tabs__list", True )
                , ( "ui-tabs__list--full", config.full )
                ]
            ]
            (List.map viewItem config.items)
        ]


viewItem : Item msg -> Html msg
viewItem (Item entry) =
    li [ class "ui-tabs__item" ]
        [ a
            (href entry.url
                :: classList [ ( "ui-tabs__tab", True ), ( "is-active", entry.active ) ]
                :: (if entry.active then
                        [ attribute "aria-current" "page" ]

                    else
                        []
                   )
                ++ TestId.attributes entry.testId
            )
            (Maybe.values
                [ Maybe.map Icon.toHtml entry.icon
                , Just (span [ class "ui-tabs__label", attribute "data-label" entry.label ] [ text entry.label ])
                , entry.badge
                ]
            )
        ]
