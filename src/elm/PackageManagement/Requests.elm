module PackageManagement.Requests exposing (..)

import Auth.Models exposing (Session)
import FileReader exposing (NativeFile)
import Http
import Json.Decode as Decode
import Jwt
import PackageManagement.Models exposing (Package, PackageDetail, packageDetailListDecoder, packageListDecoder)
import Requests exposing (apiUrl)


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


importPackage : NativeFile -> Session -> Http.Request Decode.Value
importPackage file session =
    let
        body =
            Http.multipartBody [ FileReader.filePart "file" file ]
    in
    Jwt.post session.token (apiUrl "/packages/import") body Decode.value
