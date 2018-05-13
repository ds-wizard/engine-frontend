module KMPackages.Index.Msgs exposing (..)

import Jwt
import KMPackages.Models exposing (Package)


type Msg
    = GetPackagesCompleted (Result Jwt.JwtError (List Package))
    | ShowHideDeletePackage (Maybe Package)
    | DeletePackage
    | DeletePackageCompleted (Result Jwt.JwtError String)
