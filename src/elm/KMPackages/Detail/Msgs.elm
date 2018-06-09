module KMPackages.Detail.Msgs exposing (..)

import Bootstrap.Dropdown as Dropdown
import Jwt
import KMPackages.Common.Models exposing (PackageDetail)


type Msg
    = GetPackageCompleted (Result Jwt.JwtError (List PackageDetail))
    | ShowHideDeleteVersion (Maybe String)
    | DeleteVersion
    | DeleteVersionCompleted (Result Jwt.JwtError String)
    | DropdownMsg PackageDetail Dropdown.State
