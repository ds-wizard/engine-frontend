module PackageManagement.Detail.Msgs exposing (..)

{-|

@docs Msg

-}

import Jwt
import PackageManagement.Models exposing (PackageDetail)


{-| -}
type Msg
    = GetPackageCompleted (Result Jwt.JwtError (List PackageDetail))
    | ShowHideDeleteDialog Bool
    | DeletePackage
    | DeletePackageCompleted (Result Jwt.JwtError String)
    | ShowHideDeleteVersion (Maybe String)
    | DeleteVersion
    | DeleteVersionCompleted (Result Jwt.JwtError String)
