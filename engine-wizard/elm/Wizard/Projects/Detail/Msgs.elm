module Wizard.Projects.Detail.Msgs exposing (Msg(..))

import Shared.Data.Questionnaire.QuestionnaireTodo exposing (QuestionnaireTodo)
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent exposing (QuestionnaireEvent)
import Shared.Error.ApiError exposing (ApiError)
import Shared.WebSocket as WebSocket
import Time
import Uuid exposing (Uuid)
import Wizard.Common.Components.OnlineUser as OnlineUser
import Wizard.Common.Components.Questionnaire as Questionnaire
import Wizard.Common.Components.SummaryReport as SummaryReport
import Wizard.Projects.Detail.Components.NewDocument as NewDocument
import Wizard.Projects.Detail.Components.PlanSaving as PlanSaving
import Wizard.Projects.Detail.Components.Preview as Preview
import Wizard.Projects.Detail.Components.QuestionnaireVersionViewModal as QuestionnaireVersionViewModal
import Wizard.Projects.Detail.Components.RevertModal as RevertModal
import Wizard.Projects.Detail.Components.Settings as Settings
import Wizard.Projects.Detail.Components.ShareModal as ShareModal
import Wizard.Projects.Detail.Documents.Msgs as Documents


type Msg
    = GetQuestionnaireComplete (Result ApiError QuestionnaireDetail)
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
    | QuestionnaireVersionViewModalMsg QuestionnaireVersionViewModal.Msg
    | OpenVersionPreview Uuid Uuid
    | RevertModalMsg RevertModal.Msg
    | OpenRevertModal QuestionnaireEvent
    | AddToMyProjects
    | PutQuestionnaireComplete (Result ApiError ())
    | ResetModel
