module Wizard.KMEditor.Index.Subscriptions exposing (..)

import ActionResult exposing (ActionResult(..))
import Wizard.Common.Components.Listing as Listing
import Wizard.KMEditor.Index.Models exposing (Model)
import Wizard.KMEditor.Index.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.branches of
        Success branches ->
            Sub.map ListingMsg <| Listing.subscriptions branches

        _ ->
            Sub.none
