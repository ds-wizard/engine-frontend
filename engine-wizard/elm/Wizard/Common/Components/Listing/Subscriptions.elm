module Wizard.Common.Components.Listing.Subscriptions exposing (..)

import Bootstrap.Dropdown as Dropdown
import Wizard.Common.Components.Listing.Models exposing (Model)
import Wizard.Common.Components.Listing.Msgs exposing (Msg(..))


subscriptions : Model a -> Sub (Msg a)
subscriptions model =
    let
        itemSubscription index item =
            Dropdown.subscriptions item.dropdownState (ItemDropdownMsg index)

        sortSubscription =
            Dropdown.subscriptions model.sortDropdownState SortDropdownMsg
    in
    Sub.batch <|
        (sortSubscription :: List.indexedMap itemSubscription model.items)
