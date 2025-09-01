module Wizard.Dev.Subscriptions exposing (subscriptions)

import Wizard.Dev.Models exposing (Model)
import Wizard.Dev.Msgs exposing (Msg(..))
import Wizard.Dev.PersistentCommandsDetail.Subscriptions
import Wizard.Dev.PersistentCommandsIndex.Subscriptions
import Wizard.Dev.Routes exposing (Route(..))


subscriptions : Route -> Model -> Sub Msg
subscriptions route model =
    case route of
        PersistentCommandsDetail _ ->
            Sub.map PersistentCommandsDetailMsg <| Wizard.Dev.PersistentCommandsDetail.Subscriptions.subscriptions model.persistentCommandsDetailModel

        PersistentCommandsIndex _ _ ->
            Sub.map PersistentCommandsIndexMsg <| Wizard.Dev.PersistentCommandsIndex.Subscriptions.subscriptions model.persistentCommandsIndexModel

        _ ->
            Sub.none
