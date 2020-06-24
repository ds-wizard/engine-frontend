module Wizard.KMEditor.Editor.KMEditor.Models.EditorContext exposing (EditorContext)

import Shared.Data.KnowledgeModel.Level exposing (Level)
import Shared.Data.KnowledgeModel.Metric exposing (Metric)


type alias EditorContext =
    { metrics : List Metric
    , levels : List Level
    }
