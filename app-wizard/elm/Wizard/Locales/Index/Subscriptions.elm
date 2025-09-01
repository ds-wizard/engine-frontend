module Wizard.Locales.Index.Subscriptions exposing (subscriptions)

import Wizard.Common.Components.Listing.Subscriptions as Listing
import Wizard.Locales.Index.Models exposing (Model)
import Wizard.Locales.Index.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ListingMsg <|
        Listing.subscriptions model.locales
