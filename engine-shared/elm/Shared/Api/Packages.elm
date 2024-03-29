module Shared.Api.Packages exposing
    ( deletePackage
    , deletePackageVersion
    , exportPackageUrl
    , getOutdatedPackages
    , getPackage
    , getPackageWithoutDeprecatedVersions
    , getPackages
    , getPackagesSuggestions
    , getPackagesSuggestionsWithOptions
    , importFromOwl
    , importPackage
    , postFromBranch
    , postFromMigration
    , pullPackage
    , putPackage
    )

import File exposing (File)
import Http
import Json.Encode as E
import Maybe.Extra as Maybe
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, jwtDelete, jwtFetch, jwtGet, jwtOrHttpGet, jwtPostEmpty, jwtPostFile, jwtPostFileWithData, jwtPut)
import Shared.Data.Package as Package exposing (Package)
import Shared.Data.Package.PackagePhase as PackagePhase exposing (PackagePhase)
import Shared.Data.PackageDetail as PackageDetail exposing (PackageDetail)
import Shared.Data.PackageSuggestion as PackageSuggestion exposing (PackageSuggestion)
import Shared.Data.Pagination as Pagination exposing (Pagination)
import Shared.Data.PaginationQueryFilters exposing (PaginationQueryFilters)
import Shared.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Shared.Utils exposing (boolToString)
import Uuid exposing (Uuid)


getPackages : PaginationQueryFilters -> PaginationQueryString -> AbstractAppState a -> ToMsg (Pagination Package) msg -> Cmd msg
getPackages _ qs =
    let
        queryString =
            PaginationQueryString.toApiUrl qs

        url =
            "/packages" ++ queryString
    in
    jwtGet url (Pagination.decoder "packages" Package.decoder)


getOutdatedPackages : AbstractAppState a -> ToMsg (Pagination Package) msg -> Cmd msg
getOutdatedPackages =
    let
        queryString =
            PaginationQueryString.empty
                |> PaginationQueryString.withSize (Just 5)
                |> PaginationQueryString.toApiUrlWith [ ( "outdated", "true" ) ]

        url =
            "/packages" ++ queryString
    in
    jwtGet url (Pagination.decoder "packages" Package.decoder)


getPackagesSuggestions : Maybe Bool -> PaginationQueryString -> AbstractAppState a -> ToMsg (Pagination PackageSuggestion) msg -> Cmd msg
getPackagesSuggestions nonEditable qs =
    let
        queryString =
            PaginationQueryString.toApiUrlWith
                [ ( "phase", PackagePhase.toString PackagePhase.Released )
                , ( "nonEditable", Maybe.unwrap "" boolToString nonEditable )
                ]
                qs

        url =
            "/packages/suggestions" ++ queryString
    in
    jwtGet url (Pagination.decoder "packages" PackageSuggestion.decoder)


getPackagesSuggestionsWithOptions : PaginationQueryString -> List String -> List String -> AbstractAppState a -> ToMsg (Pagination PackageSuggestion) msg -> Cmd msg
getPackagesSuggestionsWithOptions qs select exclude =
    let
        queryString =
            PaginationQueryString.toApiUrlWith
                [ ( "select", String.join "," select )
                , ( "exclude", String.join "," exclude )
                ]
                qs

        url =
            "/packages/suggestions" ++ queryString
    in
    jwtGet url (Pagination.decoder "packages" PackageSuggestion.decoder)


getPackage : String -> AbstractAppState a -> ToMsg PackageDetail msg -> Cmd msg
getPackage packageId =
    jwtOrHttpGet ("/packages/" ++ packageId) PackageDetail.decoder


getPackageWithoutDeprecatedVersions : String -> AbstractAppState a -> ToMsg PackageDetail msg -> Cmd msg
getPackageWithoutDeprecatedVersions packageId =
    jwtGet ("/packages/" ++ packageId ++ "?excludeDeprecatedVersions=true") PackageDetail.decoder


postFromBranch : Uuid -> AbstractAppState a -> ToMsg Package msg -> Cmd msg
postFromBranch uuid =
    let
        body =
            E.object [ ( "branchUuid", Uuid.encode uuid ) ]
    in
    jwtFetch "/packages/from-branch" Package.decoder body


postFromMigration : E.Value -> AbstractAppState a -> ToMsg Package msg -> Cmd msg
postFromMigration =
    jwtFetch "/packages/from-migration" Package.decoder


putPackage : { p | id : String, phase : PackagePhase } -> AbstractAppState a -> ToMsg () msg -> Cmd msg
putPackage package =
    let
        body =
            PackageDetail.encode package
    in
    jwtPut ("/packages/" ++ package.id) body


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


importFromOwl : List ( String, String ) -> File -> AbstractAppState a -> ToMsg () msg -> Cmd msg
importFromOwl params =
    let
        httpParams =
            List.map (\( k, v ) -> Http.stringPart k v) params
    in
    jwtPostFileWithData "/packages/bundle" httpParams


exportPackageUrl : String -> AbstractAppState a -> String
exportPackageUrl packageId appState =
    appState.apiUrl ++ "/packages/" ++ packageId ++ "/bundle"
