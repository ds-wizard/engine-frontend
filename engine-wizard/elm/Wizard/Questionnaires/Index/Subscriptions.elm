module Wizard.Questionnaires.Index.Subscriptions exposing (..)

import ActionResult exposing (ActionResult(..))
import Wizard.Common.Components.Listing as Listing
import Wizard.Questionnaires.Index.Models exposing (Model)
import Wizard.Questionnaires.Index.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.questionnaires of
        Success questionnaires ->
            Sub.map ListingMsg <| Listing.subscriptions questionnaires

        _ ->
            Sub.none
