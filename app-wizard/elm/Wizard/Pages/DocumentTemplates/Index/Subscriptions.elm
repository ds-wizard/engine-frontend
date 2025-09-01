module Wizard.Pages.DocumentTemplates.Index.Subscriptions exposing (subscriptions)

import Wizard.Components.Listing.Subscriptions as Listing
import Wizard.Pages.DocumentTemplates.Index.Models exposing (Model)
import Wizard.Pages.DocumentTemplates.Index.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ListingMsg <|
        Listing.subscriptions model.documentTemplates
