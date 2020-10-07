module Wizard.KnowledgeModels.Index.Subscriptions exposing (..)

import Wizard.Common.Components.Listing.Subscriptions as Listing
import Wizard.KnowledgeModels.Index.Models exposing (Model)
import Wizard.KnowledgeModels.Index.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ListingMsg <|
        Listing.subscriptions model.packages
