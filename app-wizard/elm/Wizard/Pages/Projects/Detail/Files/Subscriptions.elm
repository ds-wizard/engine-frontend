module Wizard.Pages.Projects.Detail.Files.Subscriptions exposing (subscriptions)

import Wizard.Components.Listing.Subscriptions as Listing
import Wizard.Pages.Projects.Detail.Files.Models exposing (Model)
import Wizard.Pages.Projects.Detail.Files.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ListingMsg <|
        Listing.subscriptions model.projectFiles
