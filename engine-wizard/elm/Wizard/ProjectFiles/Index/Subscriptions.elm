module Wizard.ProjectFiles.Index.Subscriptions exposing (subscriptions)

import Wizard.Common.Components.Listing.Subscriptions as Listing
import Wizard.ProjectFiles.Index.Models exposing (Model)
import Wizard.ProjectFiles.Index.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ListingMsg <|
        Listing.subscriptions model.questionnaireFiles
