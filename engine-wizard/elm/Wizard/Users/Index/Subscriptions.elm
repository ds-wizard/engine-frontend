module Wizard.Users.Index.Subscriptions exposing (..)

import Wizard.Common.Components.Listing.Subscriptions as Listing
import Wizard.Users.Index.Models exposing (Model)
import Wizard.Users.Index.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ListingMsg <|
        Listing.subscriptions model.users
