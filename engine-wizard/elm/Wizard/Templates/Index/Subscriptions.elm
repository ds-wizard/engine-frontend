module Wizard.Templates.Index.Subscriptions exposing (..)

import ActionResult exposing (ActionResult(..))
import Wizard.Common.Components.Listing as Listing
import Wizard.Templates.Index.Models exposing (Model)
import Wizard.Templates.Index.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.templates of
        Success packages ->
            Sub.map ListingMsg <| Listing.subscriptions packages

        _ ->
            Sub.none
