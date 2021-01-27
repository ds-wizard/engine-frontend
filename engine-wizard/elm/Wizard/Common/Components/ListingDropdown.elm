module Wizard.Common.Components.ListingDropdown exposing (dropdown)

import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Html exposing (Html)
import Html.Attributes exposing (class)
import Shared.Html exposing (faSet)
import Wizard.Common.AppState exposing (AppState)


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
