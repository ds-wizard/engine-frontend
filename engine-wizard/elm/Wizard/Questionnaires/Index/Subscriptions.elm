module Wizard.Questionnaires.Index.Subscriptions exposing (..)

import Wizard.Common.Components.Listing.Subscriptions as Listing
import Wizard.Questionnaires.Index.Models exposing (Model)
import Wizard.Questionnaires.Index.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ListingMsg <|
        Listing.subscriptions model.questionnaires
