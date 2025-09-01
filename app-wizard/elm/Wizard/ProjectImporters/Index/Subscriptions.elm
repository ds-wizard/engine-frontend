module Wizard.ProjectImporters.Index.Subscriptions exposing (subscriptions)

import Wizard.Common.Components.Listing.Subscriptions as Listing
import Wizard.ProjectImporters.Index.Models exposing (Model)
import Wizard.ProjectImporters.Index.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ListingMsg <|
        Listing.subscriptions model.questionnaireImporters
