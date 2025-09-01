module Wizard.Pages.KMEditor.Editor.Msgs exposing (Msg(..))

import Debounce
import Shared.Api.WebSocket as WebSocket
import Shared.Data.ApiError exposing (ApiError)
import Shared.Data.Prefab exposing (Prefab)
import Uuid exposing (Uuid)
import Wizard.Api.Models.BranchDetail exposing (BranchDetail)
import Wizard.Api.Models.Event exposing (Event)
import Wizard.Api.Models.Event.CommonEventData exposing (CommonEventData)
import Wizard.Api.Models.KnowledgeModel.Integration exposing (Integration)
import Wizard.Api.Models.KnowledgeModelSecret exposing (KnowledgeModelSecret)
import Wizard.Pages.KMEditor.Editor.Components.KMEditor as KMEditor
import Wizard.Pages.KMEditor.Editor.Components.PhaseEditor as PhaseEditor
import Wizard.Pages.KMEditor.Editor.Components.Preview as Preview
import Wizard.Pages.KMEditor.Editor.Components.PublishModal as PublishModal
import Wizard.Pages.KMEditor.Editor.Components.Settings as Settings
import Wizard.Pages.KMEditor.Editor.Components.TagEditor as TagEditor
import Wizard.Pages.Projects.Detail.Components.ProjectSaving as ProjectSaving


type Msg
    = GetBranchComplete (Result ApiError BranchDetail)
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
    | EventMsg Bool (Maybe String) String (Maybe String) (CommonEventData -> Event)
    | EventDebounceMsg String Debounce.Msg
    | EventAddSavingUuid Uuid String
    | SavePreviewReplies
    | ResetModel
