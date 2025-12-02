module Wizard.Pages.KMEditor.Editor.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Common.Api.Models.Prefab exposing (Prefab)
import Common.Api.WebSocket as WebSocket
import Debounce
import Uuid exposing (Uuid)
import Wizard.Api.Models.Event.EventContent exposing (EventContent)
import Wizard.Api.Models.KnowledgeModel.Integration exposing (Integration)
import Wizard.Api.Models.KnowledgeModelEditorDetail exposing (KnowledgeModelEditorDetail)
import Wizard.Api.Models.KnowledgeModelSecret exposing (KnowledgeModelSecret)
import Wizard.Pages.KMEditor.Editor.Components.KMEditor as KMEditor
import Wizard.Pages.KMEditor.Editor.Components.PhaseEditor as PhaseEditor
import Wizard.Pages.KMEditor.Editor.Components.Preview as Preview
import Wizard.Pages.KMEditor.Editor.Components.PublishModal as PublishModal
import Wizard.Pages.KMEditor.Editor.Components.Settings as Settings
import Wizard.Pages.KMEditor.Editor.Components.TagEditor as TagEditor
import Wizard.Pages.Projects.Detail.Components.ProjectSaving as ProjectSaving


type Msg
    = GetKnowledgeModelEditorComplete (Result ApiError KnowledgeModelEditorDetail)
    | GetIntegrationPrefabsComplete (Result ApiError (List (Prefab Integration)))
    | GetKnowledgeModelSecretsComplete (Result ApiError (List KnowledgeModelSecret))
    | WebSocketMsg WebSocket.RawMsg
    | WebSocketPing
    | SavingMsg ProjectSaving.Msg
    | Refresh
    | KMEditorMsg KMEditor.Msg
    | PhaseEditorMsg PhaseEditor.Msg
    | TagEditorMsg TagEditor.Msg
    | PreviewMsg Preview.Msg
    | SettingsMsg Settings.Msg
    | PublishModalMsg PublishModal.Msg
    | EventMsg Bool (Maybe String) (Maybe Int) String (Maybe String) EventContent
    | EventDebounceMsg String Debounce.Msg
    | EventAddSavingUuid Uuid String
    | ComputeWarnings
    | ComputeWarningsDebounceMsg Debounce.Msg
    | SavePreviewReplies
    | ResetModel
