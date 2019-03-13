module KnowledgeModels.Requests exposing (deletePackage, deletePackageVersion, exportPackageUrl, getPackages, getPackagesFiltered, getPackagesUnique, importPackage)

import Auth.Models exposing (Session)
import Http exposing (stringPart)
import Json.Decode as Decode
import Jwt
import KnowledgeModels.Common.Models exposing (..)
import Ports exposing (FilePortData)
import Requests exposing (apiUrl)


getPackagesUnique : Session -> Http.Request (List Package)
getPackagesUnique session =
    Requests.get session "/packages/unique" packageListDecoder


getPackages : Session -> Http.Request (List PackageDetail)
getPackages session =
    Requests.get session "/packages" packageDetailListDecoder


getPackagesFiltered : String -> String -> Session -> Http.Request (List PackageDetail)
getPackagesFiltered organizationId kmId session =
    Requests.get session ("/packages/?organizationId=" ++ organizationId ++ "&kmId=" ++ kmId) packageDetailListDecoder


deletePackage : String -> String -> Session -> Http.Request String
deletePackage organizationId kmId session =
    Requests.delete session ("/packages/?organizationId=" ++ organizationId ++ "&kmId=" ++ kmId)


deletePackageVersion : String -> Session -> Http.Request String
deletePackageVersion packageId session =
    Requests.delete session ("/packages/" ++ packageId)


importPackage : FilePortData -> Session -> Http.Request Decode.Value
importPackage file session =
    let
        body =
            Http.stringBody "application/json" file.contents
    in
    Jwt.post session.token (apiUrl "/packages") body Decode.value


exportPackageUrl : String -> String
exportPackageUrl packageId =
    apiUrl <| "/export/" ++ packageId
