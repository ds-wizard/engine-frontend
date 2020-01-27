module Wizard.KnowledgeModels.Index.Subscriptions exposing (..)

import ActionResult exposing (ActionResult(..))
import Wizard.Common.Components.Listing as Listing
import Wizard.KnowledgeModels.Index.Models exposing (Model)
import Wizard.KnowledgeModels.Index.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.packages of
        Success packages ->
            Sub.map ListingMsg <| Listing.subscriptions packages

        _ ->
            Sub.none
