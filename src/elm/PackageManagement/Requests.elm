module PackageManagement.Requests exposing (..)

import Auth.Models exposing (Session)
import Http
import PackageManagement.Models exposing (Package, packageListDecoder)
import Requests


getPackages : Session -> Http.Request (List Package)
getPackages session =
    Requests.get session "/packages" packageListDecoder
