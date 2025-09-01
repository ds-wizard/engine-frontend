module Wizard.Pages.ProjectImporters.Index.Subscriptions exposing (subscriptions)

import Wizard.Components.Listing.Subscriptions as Listing
import Wizard.Pages.ProjectImporters.Index.Models exposing (Model)
import Wizard.Pages.ProjectImporters.Index.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ListingMsg <|
        Listing.subscriptions model.questionnaireImporters
