module KMEditor.Migration.Msgs exposing (..)

{-|

@docs Msg

-}

import Jwt
import KMEditor.Models.Migration exposing (Migration)


{-| -}
type Msg
    = GetMigrationCompleted (Result Jwt.JwtError Migration)
    | ApplyEvent
    | RejectEvent
    | PostMigrationConflictCompleted (Result Jwt.JwtError String)
