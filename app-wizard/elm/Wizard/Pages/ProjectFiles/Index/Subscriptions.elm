module Wizard.Pages.ProjectFiles.Index.Subscriptions exposing (subscriptions)

import Wizard.Components.Listing.Subscriptions as Listing
import Wizard.Pages.ProjectFiles.Index.Models exposing (Model)
import Wizard.Pages.ProjectFiles.Index.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ListingMsg <|
        Listing.subscriptions model.questionnaireFiles
