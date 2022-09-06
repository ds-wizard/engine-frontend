module Wizard.Projects.Detail.Msgs exposing (Msg(..))

import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Data.QuestionnaireDetail.QuestionnaireEvent exposing (QuestionnaireEvent)
import Shared.Data.QuestionnaireImporter exposing (QuestionnaireImporter)
import Shared.Error.ApiError exposing (ApiError)
import Shared.WebSocket as WebSocket
import Uuid exposing (Uuid)
import Wizard.Common.Components.Questionnaire as Questionnaire
import Wizard.Common.Components.SummaryReport as SummaryReport
import Wizard.Projects.Detail.Components.NewDocument as NewDocument
import Wizard.Projects.Detail.Components.Preview as Preview
import Wizard.Projects.Detail.Components.ProjectSaving as ProjectSaving
import Wizard.Projects.Detail.Components.QuestionnaireVersionViewModal as QuestionnaireVersionViewModal
import Wizard.Projects.Detail.Components.RevertModal as RevertModal
import Wizard.Projects.Detail.Components.Settings as Settings
import Wizard.Projects.Detail.Components.ShareModal as ShareModal
import Wizard.Projects.Detail.Documents.Msgs as Documents


type Msg
    = GetQuestionnaireComplete (Result ApiError QuestionnaireDetail)
    | GetQuestionnaireImportersComplete (Result ApiError (List QuestionnaireImporter))
    | WebSocketMsg WebSocket.RawMsg
    | WebSocketPing
    | ProjectSavingMsg ProjectSaving.Msg
    | PreviewMsg Preview.Msg
    | QuestionnaireMsg Questionnaire.Msg
    | SummaryReportMsg SummaryReport.Msg
    | DocumentsMsg Documents.Msg
    | NewDocumentMsg NewDocument.Msg
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
