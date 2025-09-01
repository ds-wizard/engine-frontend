module Wizard.Comments.Subscriptions exposing (subscriptions)

import Wizard.Comments.Models exposing (Model)
import Wizard.Comments.Msgs exposing (Msg(..))
import Wizard.Common.Components.Listing.Subscriptions as Listing


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ListingMsg <|
        Listing.subscriptions model.commentThreads
