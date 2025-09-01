module Wizard.Dev.PersistentCommandsIndex.Subscriptions exposing (subscriptions)

import Wizard.Common.Components.Listing.Subscriptions as Listing
import Wizard.Dev.PersistentCommandsIndex.Models exposing (Model)
import Wizard.Dev.PersistentCommandsIndex.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ListingMsg <|
        Listing.subscriptions model.persistentCommands
