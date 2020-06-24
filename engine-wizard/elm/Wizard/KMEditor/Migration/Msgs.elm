module Wizard.KMEditor.Migration.Msgs exposing (Msg(..))

import Shared.Data.KnowledgeModel.Metric exposing (Metric)
import Shared.Data.Migration exposing (Migration)
import Shared.Error.ApiError exposing (ApiError)


type Msg
    = GetMigrationCompleted (Result ApiError Migration)
    | GetMetricsCompleted (Result ApiError (List Metric))
    | ApplyEvent
    | RejectEvent
    | PostMigrationConflictCompleted (Result ApiError ())
