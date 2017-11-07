module PackageManagement.Detail.Msgs exposing (..)

import Jwt
import PackageManagement.Models exposing (PackageDetail)


type Msg
    = GetPackageCompleted (Result Jwt.JwtError PackageDetail)
