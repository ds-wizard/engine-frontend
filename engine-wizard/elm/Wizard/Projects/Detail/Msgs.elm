module Wizard.Projects.Detail.Msgs exposing (Msg(..))

import Bootstrap.Dropdown as Dropdown
import Debounce
import Shared.Api.WebSocket as WebSocket
import Shared.Data.ApiError exposing (ApiError)
import Uuid exposing (Uuid)
import Wizard.Api.Models.QuestionnaireCommon exposing (QuestionnaireCommon)
import Wizard.Api.Models.QuestionnaireDetail.QuestionnaireEvent exposing (QuestionnaireEvent)
import Wizard.Api.Models.QuestionnaireDetailWrapper exposing (QuestionnaireDetailWrapper)
import Wizard.Api.Models.QuestionnairePreview exposing (QuestionnairePreview)
import Wizard.Api.Models.QuestionnaireQuestionnaire exposing (QuestionnaireQuestionnaire)
import Wizard.Api.Models.QuestionnaireSettings exposing (QuestionnaireSettings)
import Wizard.Api.Models.SummaryReport exposing (SummaryReport)
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
import Wizard.Projects.Detail.Files.Msgs as Files


type Msg
    = GetQuestionnaireCommonCompleted (Result ApiError QuestionnaireCommon)
    | GetQuestionnaireDetailCompleted (Result ApiError (QuestionnaireDetailWrapper QuestionnaireQuestionnaire))
    | GetQuestionnaireSummaryReportCompleted (Result ApiError (QuestionnaireDetailWrapper SummaryReport))
    | GetQuestionnairePreviewCompleted (Result ApiError (QuestionnaireDetailWrapper QuestionnairePreview))
    | GetQuestionnaireSettingsCompleted (Result ApiError (QuestionnaireDetailWrapper QuestionnaireSettings))
    | QuestionnaireDebounceMsg String Debounce.Msg
    | QuestionnaireAddSavingEvent QuestionnaireEvent
    | WebSocketMsg WebSocket.RawMsg
    | WebSocketPing
    | ProjectSavingMsg ProjectSaving.Msg
    | PreviewMsg Preview.Msg
    | QuestionnaireMsg Questionnaire.Msg
    | SummaryReportMsg SummaryReport.Msg
    | DocumentsMsg Documents.Msg
    | NewDocumentMsg NewDocument.Msg
    | FilesMsg Files.Msg
    | ShareModalMsg ShareModal.Msg
    | ShareDropdownMsg Dropdown.State
    | ShareDropdownCopyLink
    | SettingsMsg Settings.Msg
    | Refresh
    | QuestionnaireVersionViewModalMsg QuestionnaireVersionViewModal.Msg
    | OpenVersionPreview Uuid Uuid
    | RevertModalMsg RevertModal.Msg
    | OpenRevertModal QuestionnaireEvent
    | AddToMyProjects
    | PutQuestionnaireComplete (Result ApiError ())
    | ResetModel
