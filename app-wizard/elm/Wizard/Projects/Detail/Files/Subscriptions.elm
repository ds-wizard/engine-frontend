module Wizard.Projects.Detail.Files.Subscriptions exposing (subscriptions)

import Wizard.Common.Components.Listing.Subscriptions as Listing
import Wizard.Projects.Detail.Files.Models exposing (Model)
import Wizard.Projects.Detail.Files.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ListingMsg <|
        Listing.subscriptions model.questionnaireFiles
