module KMEditor.Migration.Msgs exposing (Msg(..))

import Common.ApiError exposing (ApiError)
import KMEditor.Common.KnowledgeModel.Metric exposing (Metric)
import KMEditor.Common.Migration exposing (Migration)


type Msg
    = GetMigrationCompleted (Result ApiError Migration)
    | GetMetricsCompleted (Result ApiError (List Metric))
    | ApplyEvent
    | RejectEvent
    | PostMigrationConflictCompleted (Result ApiError ())
