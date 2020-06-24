module Shared.Api.Packages exposing
    ( deletePackage
    , deletePackageVersion
    , exportPackageUrl
    , getPackage
    , getPackages
    , importPackage
    , pullPackage
    )

import Json.Decode as D
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, jwtDelete, jwtGet, jwtPostEmpty, jwtPostString)
import Shared.Data.FilePortData exposing (FilePortData)
import Shared.Data.Package as Package exposing (Package)
import Shared.Data.PackageDetail as PackageDetail exposing (PackageDetail)


getPackages : AbstractAppState a -> ToMsg (List Package) msg -> Cmd msg
getPackages =
    jwtGet "/packages" (D.list Package.decoder)


getPackage : String -> AbstractAppState a -> ToMsg PackageDetail msg -> Cmd msg
getPackage packageId =
    jwtGet ("/packages/" ++ packageId) PackageDetail.decoder


deletePackage : String -> String -> AbstractAppState a -> ToMsg () msg -> Cmd msg
deletePackage organizationId kmId =
    jwtDelete ("/packages/?organizationId=" ++ organizationId ++ "&kmId=" ++ kmId)


deletePackageVersion : String -> AbstractAppState a -> ToMsg () msg -> Cmd msg
deletePackageVersion packageId =
    jwtDelete ("/packages/" ++ packageId)


pullPackage : String -> AbstractAppState a -> ToMsg () msg -> Cmd msg
pullPackage packageId =
    jwtPostEmpty ("/packages/" ++ packageId ++ "/pull")


importPackage : FilePortData -> AbstractAppState a -> ToMsg () msg -> Cmd msg
importPackage file =
    jwtPostString "/packages" "application/json" file.contents


exportPackageUrl : String -> AbstractAppState a -> String
exportPackageUrl packageId appState =
    appState.apiUrl ++ "/export/" ++ packageId
