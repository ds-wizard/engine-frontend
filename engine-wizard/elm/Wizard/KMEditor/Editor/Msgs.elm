module Wizard.KMEditor.Editor.Msgs exposing (Msg(..))

import Debounce
import Shared.Api.WebSocket as WebSocket
import Shared.Data.ApiError exposing (ApiError)
import Shared.Data.Prefab exposing (Prefab)
import Uuid exposing (Uuid)
import Wizard.Api.Models.BranchDetail exposing (BranchDetail)
import Wizard.Api.Models.Event exposing (Event)
import Wizard.Api.Models.Event.CommonEventData exposing (CommonEventData)
import Wizard.Api.Models.KnowledgeModel.Integration exposing (Integration)
import Wizard.KMEditor.Editor.Components.KMEditor as KMEditor
import Wizard.KMEditor.Editor.Components.PhaseEditor as PhaseEditor
import Wizard.KMEditor.Editor.Components.Preview as Preview
import Wizard.KMEditor.Editor.Components.PublishModal as PublishModal
import Wizard.KMEditor.Editor.Components.Settings as Settings
import Wizard.KMEditor.Editor.Components.TagEditor as TagEditor
import Wizard.Projects.Detail.Components.ProjectSaving as ProjectSaving


type Msg
    = GetBranchComplete (Result ApiError BranchDetail)
    | GetIntegrationPrefabsComplete (Result ApiError (List (Prefab Integration)))
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
