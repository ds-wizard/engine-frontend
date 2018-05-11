module KMPackages.Index.Msgs exposing (..)

{-|

@docs Msg

-}

import Jwt
import KMPackages.Models exposing (Package)


{-| -}
type Msg
    = GetPackagesCompleted (Result Jwt.JwtError (List Package))
