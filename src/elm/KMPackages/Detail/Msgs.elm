module KMPackages.Detail.Msgs exposing (..)

import Jwt
import KMPackages.Models exposing (PackageDetail)


type Msg
    = GetPackageCompleted (Result Jwt.JwtError (List PackageDetail))
    | ShowHideDeleteVersion (Maybe String)
    | DeleteVersion
    | DeleteVersionCompleted (Result Jwt.JwtError String)
