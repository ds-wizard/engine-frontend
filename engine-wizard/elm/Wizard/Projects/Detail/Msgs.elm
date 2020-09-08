module Wizard.Projects.Detail.Msgs exposing (Msg(..))

import Shared.Data.KnowledgeModel.Level exposing (Level)
import Shared.Data.KnowledgeModel.Metric exposing (Metric)
import Shared.Data.Questionnaire.QuestionnaireTodo exposing (QuestionnaireTodo)
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Error.ApiError exposing (ApiError)
import Shared.WebSocket as WebSocket
import Time
import Wizard.Common.Components.OnlineUser as OnlineUser
import Wizard.Common.Components.Questionnaire as Questionnaire
import Wizard.Common.Components.SummaryReport as SummaryReport
import Wizard.Projects.Detail.Components.NewDocument as NewDocument
import Wizard.Projects.Detail.Components.PlanSaving as PlanSaving
import Wizard.Projects.Detail.Components.Preview as Preview
import Wizard.Projects.Detail.Components.Settings as Settings
import Wizard.Projects.Detail.Components.ShareModal as ShareModal
import Wizard.Projects.Detail.Documents.Msgs as Documents


type Msg
    = GetQuestionnaireComplete (Result ApiError QuestionnaireDetail)
    | GetLevelsComplete (Result ApiError (List Level))
    | GetMetricsComplete (Result ApiError (List Metric))
    | WebSocketMsg WebSocket.RawMsg
    | WebSocketPing Time.Posix
    | OnlineUserMsg Int OnlineUser.Msg
    | PlanSavingMsg PlanSaving.Msg
    | PreviewMsg Preview.Msg
    | QuestionnaireMsg Questionnaire.Msg
    | SummaryReportMsg SummaryReport.Msg
    | DocumentsMsg Documents.Msg
    | NewDocumentMsg NewDocument.Msg
    | ScrollToTodo QuestionnaireTodo
    | ShareModalMsg ShareModal.Msg
    | SettingsMsg Settings.Msg
    | Refresh
