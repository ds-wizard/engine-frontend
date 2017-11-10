module PackageManagement.Requests exposing (..)

import Auth.Models exposing (Session)
import Http
import PackageManagement.Models exposing (Package, PackageDetail, packageDetailListDecoder, packageListDecoder)
import Requests


getPackages : Session -> Http.Request (List Package)
getPackages session =
    Requests.get session "/packages" packageListDecoder


getPackage : String -> Session -> Http.Request (List PackageDetail)
getPackage pkgName session =
    Requests.get session ("/packages/" ++ pkgName) packageDetailListDecoder
