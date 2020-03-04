module Wizard.Users.Index.Subscriptions exposing (..)

import ActionResult exposing (ActionResult(..))
import Wizard.Common.Components.Listing as Listing
import Wizard.Users.Index.Models exposing (Model)
import Wizard.Users.Index.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.users of
        Success users ->
            Sub.map ListingMsg <| Listing.subscriptions users

        _ ->
            Sub.none
