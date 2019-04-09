module KMEditor.Editor.Msgs exposing (Msg(..))

import Common.ApiError exposing (ApiError)
import KMEditor.Common.Models exposing (KnowledgeModelDetail)
import KMEditor.Common.Models.Entities exposing (KnowledgeModel, Level, Metric)
import KMEditor.Editor.KMEditor.Msgs
import KMEditor.Editor.Models exposing (EditorType)
import KMEditor.Editor.Preview.Msgs
import KMEditor.Editor.TagEditor.Msgs


type Msg
    = GetKnowledgeModelCompleted (Result ApiError KnowledgeModelDetail)
    | GetMetricsCompleted (Result ApiError (List Metric))
    | GetLevelsCompleted (Result ApiError (List Level))
    | GetPreviewCompleted (Result ApiError KnowledgeModel)
    | OpenEditor EditorType
    | KMEditorMsg KMEditor.Editor.KMEditor.Msgs.Msg
    | TagEditorMsg KMEditor.Editor.TagEditor.Msgs.Msg
    | PreviewEditorMsg KMEditor.Editor.Preview.Msgs.Msg
    | Discard
    | Save
    | SaveCompleted (Result ApiError ())
