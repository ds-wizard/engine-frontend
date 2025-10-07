module Wizard.Pages.KMEditor.Migration.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Wizard.Api.Models.Migration exposing (Migration)


type Msg
    = GetMigrationCompleted (Result ApiError Migration)
    | ApplyAll
    | ApplyEvent
    | RejectEvent
    | PostMigrationConflictCompleted (Result ApiError ())
