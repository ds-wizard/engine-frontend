module KMEditor.Editor2.KMEditor.Models.EditorContext exposing (EditorContext)

import KMEditor.Common.Models.Entities exposing (Level, Metric)


type alias EditorContext =
    { metrics : List Metric
    , levels : List Level
    }
