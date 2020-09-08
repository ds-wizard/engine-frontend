module Wizard.Questionnaires.Detail.Subscriptions exposing (subscriptions)

import Shared.WebSocket as WebSocket
import Time
import Wizard.Questionnaires.Detail.Components.QuestionnaireSaving as QuestionnaireSaving
import Wizard.Questionnaires.Detail.Models exposing (Model)
import Wizard.Questionnaires.Detail.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ WebSocket.listen WebSocketMsg
        , Time.every (30 * 1000) WebSocketPing
        , Sub.map QuestionnaireSavingMsg <| QuestionnaireSaving.subscriptions model.questionnaireSavingModel
        ]
