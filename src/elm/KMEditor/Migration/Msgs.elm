module KMEditor.Migration.Msgs exposing (..)

import Jwt
import KMEditor.Common.Models.Migration exposing (Migration)


type Msg
    = GetMigrationCompleted (Result Jwt.JwtError Migration)
    | ApplyEvent
    | RejectEvent
    | PostMigrationConflictCompleted (Result Jwt.JwtError String)
