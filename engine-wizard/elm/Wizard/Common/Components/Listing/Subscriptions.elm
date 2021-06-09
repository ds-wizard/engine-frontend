module Wizard.Common.Components.Listing.Subscriptions exposing (..)

import Bootstrap.Dropdown as Dropdown
import Dict
import Wizard.Common.Components.Listing.Models exposing (Model)
import Wizard.Common.Components.Listing.Msgs exposing (Msg(..))


subscriptions : Model a -> Sub (Msg a)
subscriptions model =
    let
        itemSubscription index item =
            Dropdown.subscriptions item.dropdownState (ItemDropdownMsg index)

        sortSubscription =
            Dropdown.subscriptions model.sortDropdownState SortDropdownMsg

        filterSubscription ( filterId, dropdownState ) =
            Dropdown.subscriptions dropdownState (FilterDropdownMsg filterId)
    in
    Sub.batch <|
        (sortSubscription
            :: List.indexedMap itemSubscription model.items
            ++ List.map filterSubscription (Dict.toList model.filterDropdownStates)
        )
