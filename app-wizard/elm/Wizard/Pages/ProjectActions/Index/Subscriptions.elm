module Wizard.Pages.ProjectActions.Index.Subscriptions exposing (subscriptions)

import Wizard.Components.Listing.Subscriptions as Listing
import Wizard.Pages.ProjectActions.Index.Models exposing (Model)
import Wizard.Pages.ProjectActions.Index.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ListingMsg <|
        Listing.subscriptions model.questionnaireActions
