module Wizard.KMEditor.Editor.Msgs exposing (Msg(..))

import Shared.Data.BranchDetail exposing (BranchDetail)
import Shared.Data.KnowledgeModel exposing (KnowledgeModel)
import Shared.Data.KnowledgeModel.Level exposing (Level)
import Shared.Data.KnowledgeModel.Metric exposing (Metric)
import Shared.Error.ApiError exposing (ApiError)
import Wizard.KMEditor.Editor.KMEditor.Msgs
import Wizard.KMEditor.Editor.Models exposing (EditorType)
import Wizard.KMEditor.Editor.Preview.Msgs
import Wizard.KMEditor.Editor.TagEditor.Msgs


type Msg
    = GetKnowledgeModelCompleted (Result ApiError BranchDetail)
    | GetMetricsCompleted (Result ApiError (List Metric))
    | GetLevelsCompleted (Result ApiError (List Level))
    | GetPreviewCompleted (Result ApiError KnowledgeModel)
    | OpenEditor EditorType
    | KMEditorMsg Wizard.KMEditor.Editor.KMEditor.Msgs.Msg
    | TagEditorMsg Wizard.KMEditor.Editor.TagEditor.Msgs.Msg
    | PreviewEditorMsg Wizard.KMEditor.Editor.Preview.Msgs.Msg
    | Discard
    | Save
    | SaveCompleted (Result ApiError ())
