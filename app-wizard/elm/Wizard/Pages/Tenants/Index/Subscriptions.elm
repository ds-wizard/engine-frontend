module Wizard.Pages.Tenants.Index.Subscriptions exposing (subscriptions)

import Wizard.Components.Listing.Subscriptions as Listing
import Wizard.Pages.Tenants.Index.Models exposing (Model)
import Wizard.Pages.Tenants.Index.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ListingMsg <|
        Listing.subscriptions model.tenants
