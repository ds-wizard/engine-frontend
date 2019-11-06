module Wizard.KMEditor.Migration.Msgs exposing (Msg(..))

import Shared.Error.ApiError exposing (ApiError)
import Wizard.KMEditor.Common.KnowledgeModel.Metric exposing (Metric)
import Wizard.KMEditor.Common.Migration exposing (Migration)


type Msg
    = GetMigrationCompleted (Result ApiError Migration)
    | GetMetricsCompleted (Result ApiError (List Metric))
    | ApplyEvent
    | RejectEvent
    | PostMigrationConflictCompleted (Result ApiError ())
