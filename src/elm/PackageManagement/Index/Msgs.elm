module PackageManagement.Index.Msgs exposing (..)

import Jwt
import PackageManagement.Models exposing (Package)


type Msg
    = GetPackagesCompleted (Result Jwt.JwtError (List Package))
