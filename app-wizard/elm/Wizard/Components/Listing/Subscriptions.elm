module Wizard.Components.Listing.Subscriptions exposing (subscriptions)

import Bootstrap.Dropdown as Dropdown
import Dict
import Wizard.Components.Listing.Models exposing (Model)
import Wizard.Components.Listing.Msgs exposing (Msg(..))


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
