module KnowledgeModels.Migration.Msgs exposing (..)

{-|

@docs Msg

-}

import Jwt
import KnowledgeModels.Models.Migration exposing (Migration)


{-| -}
type Msg
    = GetMigrationCompleted (Result Jwt.JwtError Migration)
    | ApplyEvent
    | RejectEvent
    | PostMigrationConflictCompleted (Result Jwt.JwtError String)
