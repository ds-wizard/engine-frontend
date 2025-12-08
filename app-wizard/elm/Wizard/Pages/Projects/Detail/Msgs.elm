module Wizard.Pages.Projects.Detail.Msgs exposing (Msg(..))

import Bootstrap.Dropdown as Dropdown
import Common.Api.ApiError exposing (ApiError)
import Common.Api.WebSocket as WebSocket
import Debounce
import Uuid exposing (Uuid)
import Wizard.Api.Models.ProjectCommon exposing (ProjectCommon)
import Wizard.Api.Models.ProjectDetail.ProjectEvent exposing (ProjectEvent)
import Wizard.Api.Models.ProjectDetailWrapper exposing (ProjectDetailWrapper)
import Wizard.Api.Models.ProjectPreview exposing (ProjectPreview)
import Wizard.Api.Models.ProjectQuestionnaire exposing (ProjectQuestionnaire)
import Wizard.Api.Models.ProjectSettings exposing (ProjectSettings)
import Wizard.Api.Models.SummaryReport exposing (SummaryReport)
import Wizard.Components.Questionnaire as Questionnaire
import Wizard.Components.SummaryReport as SummaryReport
import Wizard.Pages.Projects.Detail.Components.NewDocument as NewDocument
import Wizard.Pages.Projects.Detail.Components.Preview as Preview
import Wizard.Pages.Projects.Detail.Components.ProjectSaving as ProjectSaving
import Wizard.Pages.Projects.Detail.Components.ProjectVersionViewModal as ProjectVersionViewModal
import Wizard.Pages.Projects.Detail.Components.RevertModal as RevertModal
import Wizard.Pages.Projects.Detail.Components.Settings as Settings
import Wizard.Pages.Projects.Detail.Components.ShareModal as ShareModal
import Wizard.Pages.Projects.Detail.Documents.Msgs as Documents
import Wizard.Pages.Projects.Detail.Files.Msgs as Files


type Msg
    = GetQuestionnaireCommonCompleted (Result ApiError ProjectCommon)
    | GetQuestionnaireDetailCompleted (Result ApiError (ProjectDetailWrapper ProjectQuestionnaire))
    | GetQuestionnaireSummaryReportCompleted (Result ApiError (ProjectDetailWrapper SummaryReport))
    | GetQuestionnairePreviewCompleted (Result ApiError (ProjectDetailWrapper ProjectPreview))
    | GetQuestionnaireSettingsCompleted (Result ApiError (ProjectDetailWrapper ProjectSettings))
    | QuestionnaireDebounceMsg String Debounce.Msg
    | QuestionnaireAddSavingEvent ProjectEvent
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
    | ShareModalCloseMsg
    | ShareDropdownMsg Dropdown.State
    | ShareDropdownCopyLink
    | SettingsMsg Settings.Msg
    | Refresh
    | QuestionnaireVersionViewModalMsg ProjectVersionViewModal.Msg
    | OpenVersionPreview Uuid Uuid
    | RevertModalMsg RevertModal.Msg
    | OpenRevertModal ProjectEvent
    | AddToMyProjects
    | PutQuestionnaireComplete (Result ApiError ())
    | ResetModel
