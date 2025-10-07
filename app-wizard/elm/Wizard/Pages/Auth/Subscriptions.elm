module Wizard.Pages.Auth.Subscriptions exposing (subscriptions)

import Time
import Wizard.Data.AppState as AppState
import Wizard.Models exposing (Model)
import Wizard.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Wizard.Msgs.Msg
subscriptions model =
    if AppState.sessionExpiresSoon model.appState then
        Time.every 1000 OnTime

    else
        Sub.none
