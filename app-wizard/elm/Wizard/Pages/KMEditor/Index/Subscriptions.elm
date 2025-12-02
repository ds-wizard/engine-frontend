module Wizard.Pages.KMEditor.Index.Subscriptions exposing (subscriptions)

import Wizard.Components.Listing.Subscriptions as Listing
import Wizard.Pages.KMEditor.Index.Models exposing (Model)
import Wizard.Pages.KMEditor.Index.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ListingMsg <|
        Listing.subscriptions model.kmEditors
