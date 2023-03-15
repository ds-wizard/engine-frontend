module Wizard.Common.Components.ListingDropdown exposing (LinkAnchorItemCfg, MsgAnchorItemCfg, dropdown, itemsFromGroups, linkAnchorItem, msgAnchorItem)

import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Html exposing (Html, text)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import Shared.Html exposing (faSet)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Routes exposing (Route)
import Wizard.Routing as Routing


dropdown :
    AppState
    ->
        { dropdownState : Dropdown.State
        , toggleMsg : Dropdown.State -> msg
        , items : List (Dropdown.DropdownItem msg)
        }
    -> Html msg
dropdown appState { dropdownState, toggleMsg, items } =
    Dropdown.dropdown dropdownState
        { options =
            [ Dropdown.attrs [ class "ListingDropdown" ]
            , Dropdown.alignMenuRight
            ]
        , toggleMsg = toggleMsg
        , toggleButton =
            Dropdown.toggle [ Button.roleLink ]
                [ faSet "listing.actions" appState ]
        , items = items
        }


type alias LinkAnchorItemCfg msg =
    { route : Route
    , icon : Html msg
    , label : String
    , dataCy : String
    }


linkAnchorItem : AppState -> LinkAnchorItemCfg msg -> Dropdown.DropdownItem msg
linkAnchorItem appState cfg =
    Dropdown.anchorItem
        [ dataCy cfg.dataCy
        , href (Routing.toUrl appState cfg.route)
        ]
        [ cfg.icon
        , text cfg.label
        ]


type alias MsgAnchorItemCfg msg =
    { msg : msg
    , icon : Html msg
    , label : String
    , dataCy : String
    }


msgAnchorItem : MsgAnchorItemCfg msg -> Dropdown.DropdownItem msg
msgAnchorItem cfg =
    Dropdown.anchorItem
        [ dataCy cfg.dataCy
        , onClick cfg.msg
        ]
        [ cfg.icon
        , text cfg.label
        ]


itemsFromGroups : a -> List (List ( a, Bool )) -> List a
itemsFromGroups separator groups =
    let
        filter ( item, visible ) =
            if visible then
                Just item

            else
                Nothing
    in
    groups
        |> List.map (List.filterMap filter)
        |> List.filter (not << List.isEmpty)
        |> List.intersperse [ separator ]
        |> List.concat
