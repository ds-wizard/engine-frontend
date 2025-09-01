module Wizard.Pages.Dev.Subscriptions exposing (subscriptions)

import Wizard.Pages.Dev.Models exposing (Model)
import Wizard.Pages.Dev.Msgs exposing (Msg(..))
import Wizard.Pages.Dev.PersistentCommandsDetail.Subscriptions
import Wizard.Pages.Dev.PersistentCommandsIndex.Subscriptions
import Wizard.Pages.Dev.Routes exposing (Route(..))


subscriptions : Route -> Model -> Sub Msg
subscriptions route model =
    case route of
        PersistentCommandsDetail _ ->
            Sub.map PersistentCommandsDetailMsg <| Wizard.Pages.Dev.PersistentCommandsDetail.Subscriptions.subscriptions model.persistentCommandsDetailModel

        PersistentCommandsIndex _ _ ->
            Sub.map PersistentCommandsIndexMsg <| Wizard.Pages.Dev.PersistentCommandsIndex.Subscriptions.subscriptions model.persistentCommandsIndexModel

        _ ->
            Sub.none
