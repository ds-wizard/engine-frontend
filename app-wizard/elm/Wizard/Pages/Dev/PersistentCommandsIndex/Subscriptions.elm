module Wizard.Pages.Dev.PersistentCommandsIndex.Subscriptions exposing (subscriptions)

import Wizard.Components.Listing.Subscriptions as Listing
import Wizard.Pages.Dev.PersistentCommandsIndex.Models exposing (Model)
import Wizard.Pages.Dev.PersistentCommandsIndex.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ListingMsg <|
        Listing.subscriptions model.persistentCommands
