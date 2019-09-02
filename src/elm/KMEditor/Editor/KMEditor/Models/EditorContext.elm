module KMEditor.Editor.KMEditor.Models.EditorContext exposing (EditorContext)

import KMEditor.Common.KnowledgeModel.Level exposing (Level)
import KMEditor.Common.KnowledgeModel.Metric exposing (Metric)


type alias EditorContext =
    { metrics : List Metric
    , levels : List Level
    }
