module KMEditor.Editor.Msgs exposing (Msg(..))

import Jwt
import KMEditor.Common.Models exposing (Branch)
import KMEditor.Common.Models.Entities exposing (KnowledgeModel, Level, Metric)
import KMEditor.Editor.KMEditor.Msgs
import KMEditor.Editor.Models exposing (EditorType)
import KMEditor.Editor.Preview.Msgs
import KMEditor.Editor.TagEditor.Msgs


type Msg
    = GetBranchCompleted (Result Jwt.JwtError Branch)
    | GetMetricsCompleted (Result Jwt.JwtError (List Metric))
    | GetLevelsCompleted (Result Jwt.JwtError (List Level))
    | GetPreviewCompleted (Result Jwt.JwtError KnowledgeModel)
    | OpenEditor EditorType
    | KMEditorMsg KMEditor.Editor.KMEditor.Msgs.Msg
    | TagEditorMsg KMEditor.Editor.TagEditor.Msgs.Msg
    | PreviewEditorMsg KMEditor.Editor.Preview.Msgs.Msg
    | Discard
    | Save
    | SaveCompleted (Result Jwt.JwtError String)
