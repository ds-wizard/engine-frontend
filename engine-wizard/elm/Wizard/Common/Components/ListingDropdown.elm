module Wizard.Common.Components.ListingDropdown exposing
    ( ListingActionConfig
    , ListingActionType(..)
    , ListingDropdownItem(..)
    , dropdown
    , dropdownAction
    , dropdownSeparator
    , itemsFromGroups
    )

import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Html exposing (Html, text)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import Shared.Html exposing (faSet)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Routes as Routes
import Wizard.Routing as Routing


type ListingDropdownItem msg
    = ListingDropdownAction (ListingActionConfig msg)
    | ListingDropdownSeparator


type alias ListingActionConfig msg =
    { extraClass : Maybe String
    , icon : Html msg
    , label : String
    , msg : ListingActionType msg
    , dataCy : String
    }


type ListingActionType msg
    = ListingActionMsg msg
    | ListingActionLink Routes.Route


dropdownAction : ListingActionConfig msg -> ListingDropdownItem msg
dropdownAction =
    ListingDropdownAction


dropdownSeparator : ListingDropdownItem msg
dropdownSeparator =
    ListingDropdownSeparator


dropdown :
    AppState
    ->
        { dropdownState : Dropdown.State
        , toggleMsg : Dropdown.State -> msg
        , items : List (ListingDropdownItem msg)
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
        , items = List.map (viewAction appState) items
        }


viewAction : AppState -> ListingDropdownItem msg -> Dropdown.DropdownItem msg
viewAction appState dropdownItem =
    case dropdownItem of
        ListingDropdownAction action ->
            let
                attrs =
                    case action.msg of
                        ListingActionLink route ->
                            [ href <| Routing.toUrl appState route ]

                        ListingActionMsg msg ->
                            [ onClick msg ]
            in
            Dropdown.anchorItem
                ([ class <| Maybe.withDefault "" action.extraClass
                 , dataCy ("listing-item_action_" ++ action.dataCy)
                 ]
                    ++ attrs
                )
                [ action.icon, text action.label ]

        ListingDropdownSeparator ->
            Dropdown.divider


itemsFromGroups : List (List ( ListingDropdownItem msg, Bool )) -> List (ListingDropdownItem msg)
itemsFromGroups groups =
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
        |> List.intersperse [ dropdownSeparator ]
        |> List.concat
