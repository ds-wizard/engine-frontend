module Common.Api.Packages exposing (deletePackage, deletePackageVersion, exportPackageUrl, getPackages, getPackagesFiltered, getPackagesUnique, importPackage)

import Common.Api exposing (ToMsg, jwtDelete, jwtGet, jwtPostString)
import Common.AppState exposing (AppState)
import KnowledgeModels.Common.Models exposing (Package, PackageDetail, packageDetailListDecoder, packageListDecoder)
import Ports exposing (FilePortData)


getPackages : AppState -> ToMsg (List PackageDetail) msg -> Cmd msg
getPackages =
    jwtGet "/packages" packageDetailListDecoder


getPackagesUnique : AppState -> ToMsg (List Package) msg -> Cmd msg
getPackagesUnique =
    jwtGet "/packages/unique" packageListDecoder


getPackagesFiltered : String -> String -> AppState -> ToMsg (List PackageDetail) msg -> Cmd msg
getPackagesFiltered organizationId kmId =
    jwtGet ("/packages/?organizationId=" ++ organizationId ++ "&kmId=" ++ kmId) packageDetailListDecoder


deletePackage : String -> String -> AppState -> ToMsg () msg -> Cmd msg
deletePackage organizationId kmId =
    jwtDelete ("/packages/?organizationId=" ++ organizationId ++ "&kmId=" ++ kmId)


deletePackageVersion : String -> AppState -> ToMsg () msg -> Cmd msg
deletePackageVersion packageId =
    jwtDelete ("/packages/" ++ packageId)


importPackage : FilePortData -> AppState -> ToMsg () msg -> Cmd msg
importPackage file =
    jwtPostString "/packages" "application/json" file.contents


exportPackageUrl : String -> AppState -> String
exportPackageUrl packageId appState =
    appState.apiUrl ++ "/export/" ++ packageId
