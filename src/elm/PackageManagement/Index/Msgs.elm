module PackageManagement.Index.Msgs exposing (..)

{-|

@docs Msg

-}

import Jwt
import PackageManagement.Models exposing (Package)


{-| -}
type Msg
    = GetPackagesCompleted (Result Jwt.JwtError (List Package))
