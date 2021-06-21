module Wizard.KMEditor.Migration.Msgs exposing (Msg(..))

import Shared.Data.Migration exposing (Migration)
import Shared.Error.ApiError exposing (ApiError)


type Msg
    = GetMigrationCompleted (Result ApiError Migration)
    | ApplyEvent
    | RejectEvent
    | PostMigrationConflictCompleted (Result ApiError ())
