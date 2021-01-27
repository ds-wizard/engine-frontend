module Wizard.Projects.Detail.Subscriptions exposing (subscriptions)

import ActionResult exposing (ActionResult(..))
import Shared.WebSocket as WebSocket
import Time
import Wizard.Common.Components.Questionnaire as Questionnaire
import Wizard.Projects.Detail.Components.PlanSaving as PlanSaving
import Wizard.Projects.Detail.Components.Settings as Settings
import Wizard.Projects.Detail.Components.ShareModal as ShareModal
import Wizard.Projects.Detail.Documents.Subscriptions as Documents
import Wizard.Projects.Detail.Models exposing (Model)
import Wizard.Projects.Detail.Msgs exposing (Msg(..))
import Wizard.Projects.Detail.ProjectDetailRoute as PlanDetailRoute exposing (ProjectDetailRoute)


subscriptions : ProjectDetailRoute -> Model -> Sub Msg
subscriptions route model =
    let
        pageSubscriptions =
            case route of
                PlanDetailRoute.Questionnaire ->
                    case model.questionnaireModel of
                        Success questionnaireModel ->
                            Sub.map QuestionnaireMsg <|
                                Questionnaire.subscriptions questionnaireModel

                        _ ->
                            Sub.none

                PlanDetailRoute.Documents _ ->
                    Sub.map DocumentsMsg <|
                        Documents.subscriptions model.documentsModel

                PlanDetailRoute.Settings ->
                    Sub.map SettingsMsg <|
                        Settings.subscriptions model.settingsModel

                _ ->
                    Sub.none
    in
    Sub.batch
        [ WebSocket.listen WebSocketMsg
        , Time.every (30 * 1000) WebSocketPing
        , Sub.map PlanSavingMsg <| PlanSaving.subscriptions model.planSavingModel
        , Sub.map ShareModalMsg <| ShareModal.subscriptions model.shareModalModel
        , pageSubscriptions
        ]
