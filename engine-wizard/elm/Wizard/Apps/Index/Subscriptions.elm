module Wizard.Apps.Index.Subscriptions exposing (subscriptions)

import Wizard.Apps.Index.Models exposing (Model)
import Wizard.Apps.Index.Msgs exposing (Msg(..))
import Wizard.Common.Components.Listing.Subscriptions as Listing


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ListingMsg <|
        Listing.subscriptions model.apps
