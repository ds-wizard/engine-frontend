module Wizard.Pages.Users.Index.Subscriptions exposing (subscriptions)

import Wizard.Components.Listing.Subscriptions as Listing
import Wizard.Pages.Users.Index.Models exposing (Model)
import Wizard.Pages.Users.Index.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ListingMsg <|
        Listing.subscriptions model.users
