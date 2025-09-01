module Wizard.DocumentTemplates.Index.Subscriptions exposing (subscriptions)

import Wizard.Common.Components.Listing.Subscriptions as Listing
import Wizard.DocumentTemplates.Index.Models exposing (Model)
import Wizard.DocumentTemplates.Index.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ListingMsg <|
        Listing.subscriptions model.documentTemplates
