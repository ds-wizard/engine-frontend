module Wizard.Pages.KnowledgeModels.Index.Subscriptions exposing (subscriptions)

import Wizard.Components.Listing.Subscriptions as Listing
import Wizard.Pages.KnowledgeModels.Index.Models exposing (Model)
import Wizard.Pages.KnowledgeModels.Index.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ListingMsg <|
        Listing.subscriptions model.packages
