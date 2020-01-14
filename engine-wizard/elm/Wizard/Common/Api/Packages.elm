module Wizard.Common.Api.Packages exposing
    ( deletePackage
    , deletePackageVersion
    , exportPackageUrl
    , getPackage
    , getPackages
    , importPackage
    , pullPackage
    )

import Json.Decode as D
import Wizard.Common.Api exposing (ToMsg, jwtDelete, jwtGet, jwtPostEmpty, jwtPostString)
import Wizard.Common.AppState exposing (AppState)
import Wizard.KnowledgeModels.Common.Package as Package exposing (Package)
import Wizard.KnowledgeModels.Common.PackageDetail as PackageDetail exposing (PackageDetail)
import Wizard.Ports as Ports exposing (FilePortData)


getPackages : AppState -> ToMsg (List Package) msg -> Cmd msg
getPackages =
    jwtGet "/packages" (D.list Package.decoder)


getPackage : String -> AppState -> ToMsg PackageDetail msg -> Cmd msg
getPackage packageId =
    jwtGet ("/packages/" ++ packageId) PackageDetail.decoder


deletePackage : String -> String -> AppState -> ToMsg () msg -> Cmd msg
deletePackage organizationId kmId =
    jwtDelete ("/packages/?organizationId=" ++ organizationId ++ "&kmId=" ++ kmId)


deletePackageVersion : String -> AppState -> ToMsg () msg -> Cmd msg
deletePackageVersion packageId =
    jwtDelete ("/packages/" ++ packageId)


pullPackage : String -> AppState -> ToMsg () msg -> Cmd msg
pullPackage packageId =
    jwtPostEmpty ("/packages/" ++ packageId ++ "/pull")


importPackage : FilePortData -> AppState -> ToMsg () msg -> Cmd msg
importPackage file =
    jwtPostString "/packages" "application/json" file.contents


exportPackageUrl : String -> AppState -> String
exportPackageUrl packageId appState =
    appState.apiUrl ++ "/export/" ++ packageId
