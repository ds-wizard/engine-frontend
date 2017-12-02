module KnowledgeModels.Migration.Msgs exposing (..)

import Jwt
import KnowledgeModels.Models.Migration exposing (Migration)


type Msg
    = GetMigrationCompleted (Result Jwt.JwtError Migration)
    | ApplyEvent
    | RejectEvent
    | PostMigrationConflictCompleted (Result Jwt.JwtError String)
