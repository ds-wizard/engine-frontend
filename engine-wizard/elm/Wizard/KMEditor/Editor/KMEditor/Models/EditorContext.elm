module Wizard.KMEditor.Editor.KMEditor.Models.EditorContext exposing (EditorContext)

import Wizard.KMEditor.Common.KnowledgeModel.Level exposing (Level)
import Wizard.KMEditor.Common.KnowledgeModel.Metric exposing (Metric)


type alias EditorContext =
    { metrics : List Metric
    , levels : List Level
    }
