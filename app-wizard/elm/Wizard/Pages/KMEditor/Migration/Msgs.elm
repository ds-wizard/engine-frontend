module Wizard.Pages.KMEditor.Migration.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Wizard.Api.Models.KnowledgeModelMigration exposing (KnowledgeModelMigration)


type Msg
    = GetMigrationCompleted (Result ApiError KnowledgeModelMigration)
    | ApplyAll
    | ApplyEvent
    | RejectEvent
    | PostMigrationConflictCompleted (Result ApiError ())
