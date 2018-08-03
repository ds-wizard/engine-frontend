module KMEditor.Editor.Models.EditorContext exposing (..)

import KMEditor.Common.Models.Entities exposing (Metric)


type alias EditorContext =
    { metrics : List Metric
    }
