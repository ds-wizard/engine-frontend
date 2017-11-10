module PackageManagement.Requests exposing (..)

import Auth.Models exposing (Session)
import Http
import PackageManagement.Models exposing (Package, PackageDetail, packageDetailListDecoder, packageListDecoder)
import Requests


getPackages : Session -> Http.Request (List Package)
getPackages session =
    Requests.get session "/packages" packageListDecoder


getPackage : String -> Session -> Http.Request (List PackageDetail)
getPackage shortName session =
    Requests.get session ("/packages/" ++ shortName) packageDetailListDecoder


deletePackage : String -> Session -> Http.Request String
deletePackage shortName session =
    Requests.delete session ("/packages/" ++ shortName)


deletePackageVersion : String -> String -> Session -> Http.Request String
deletePackageVersion shortName version session =
    Requests.delete session ("/packages/" ++ shortName ++ "/versions/" ++ version)
