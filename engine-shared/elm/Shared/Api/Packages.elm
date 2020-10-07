module Shared.Api.Packages exposing
    ( deletePackage
    , deletePackageVersion
    , exportPackageUrl
    , getPackage
    , getPackages
    , getPackagesPaginated
    , importPackage
    , pullPackage
    )

import File exposing (File)
import Json.Decode as D
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, jwtDelete, jwtGet, jwtPostEmpty, jwtPostFile)
import Shared.Data.Package as Package exposing (Package)
import Shared.Data.PackageDetail as PackageDetail exposing (PackageDetail)
import Shared.Data.Pagination as Pagination exposing (Pagination)
import Shared.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)


getPackages : AbstractAppState a -> ToMsg (List Package) msg -> Cmd msg
getPackages =
    jwtGet "/packages" (D.list Package.decoder)


getPackagesPaginated : PaginationQueryString -> AbstractAppState a -> ToMsg (Pagination Package) msg -> Cmd msg
getPackagesPaginated qs =
    let
        queryString =
            PaginationQueryString.toApiUrl qs

        url =
            "/packages/page" ++ queryString
    in
    jwtGet url (Pagination.decoder "packages" Package.decoder)


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


importPackage : File -> AbstractAppState a -> ToMsg () msg -> Cmd msg
importPackage =
    jwtPostFile "/packages/bundle"


exportPackageUrl : String -> AbstractAppState a -> String
exportPackageUrl packageId appState =
    appState.apiUrl ++ "/packages/" ++ packageId ++ "/bundle"
