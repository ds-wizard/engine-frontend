module Wizard.Api.Packages exposing
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

import Bool.Extra as Bool
import File exposing (File)
import Http
import Json.Encode as E
import Maybe.Extra as Maybe
import Shared.Api.Request as Request exposing (ToMsg)
import Shared.Data.Pagination as Pagination exposing (Pagination)
import Shared.Data.PaginationQueryFilters exposing (PaginationQueryFilters)
import Shared.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Uuid exposing (Uuid)
import Wizard.Api.Models.Package as Package exposing (Package)
import Wizard.Api.Models.Package.PackagePhase as PackagePhase exposing (PackagePhase)
import Wizard.Api.Models.PackageDetail as PackageDetail exposing (PackageDetail)
import Wizard.Api.Models.PackageSuggestion as PackageSuggestion exposing (PackageSuggestion)
import Wizard.Common.AppState as AppState exposing (AppState)


getPackages : AppState -> PaginationQueryFilters -> PaginationQueryString -> ToMsg (Pagination Package) msg -> Cmd msg
getPackages appState _ qs =
    let
        queryString =
            PaginationQueryString.toApiUrl qs

        url =
            "/packages" ++ queryString
    in
    Request.get (AppState.toServerInfo appState) url (Pagination.decoder "packages" Package.decoder)


getOutdatedPackages : AppState -> ToMsg (Pagination Package) msg -> Cmd msg
getOutdatedPackages appState =
    let
        queryString =
            PaginationQueryString.empty
                |> PaginationQueryString.withSize (Just 5)
                |> PaginationQueryString.toApiUrlWith [ ( "outdated", "true" ) ]

        url =
            "/packages" ++ queryString
    in
    Request.get (AppState.toServerInfo appState) url (Pagination.decoder "packages" Package.decoder)


getPackagesSuggestions : AppState -> Maybe Bool -> PaginationQueryString -> ToMsg (Pagination PackageSuggestion) msg -> Cmd msg
getPackagesSuggestions appState nonEditable qs =
    let
        queryString =
            PaginationQueryString.toApiUrlWith
                [ ( "phase", PackagePhase.toString PackagePhase.Released )
                , ( "nonEditable", Maybe.unwrap "" Bool.toString nonEditable )
                ]
                qs

        url =
            "/packages/suggestions" ++ queryString
    in
    Request.get (AppState.toServerInfo appState) url (Pagination.decoder "packages" PackageSuggestion.decoder)


getPackagesSuggestionsWithOptions : AppState -> PaginationQueryString -> List String -> List String -> ToMsg (Pagination PackageSuggestion) msg -> Cmd msg
getPackagesSuggestionsWithOptions appState qs select exclude =
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
    Request.get (AppState.toServerInfo appState) url (Pagination.decoder "packages" PackageSuggestion.decoder)


getPackage : AppState -> String -> ToMsg PackageDetail msg -> Cmd msg
getPackage appState packageId =
    Request.get (AppState.toServerInfo appState) ("/packages/" ++ packageId) PackageDetail.decoder


getPackageWithoutDeprecatedVersions : AppState -> String -> ToMsg PackageDetail msg -> Cmd msg
getPackageWithoutDeprecatedVersions appState packageId =
    Request.get (AppState.toServerInfo appState) ("/packages/" ++ packageId ++ "?excludeDeprecatedVersions=true") PackageDetail.decoder


postFromBranch : AppState -> Uuid -> ToMsg Package msg -> Cmd msg
postFromBranch appState uuid =
    let
        body =
            E.object [ ( "branchUuid", Uuid.encode uuid ) ]
    in
    Request.post (AppState.toServerInfo appState) "/packages/from-branch" Package.decoder body


postFromMigration : AppState -> E.Value -> ToMsg Package msg -> Cmd msg
postFromMigration appState body =
    Request.post (AppState.toServerInfo appState) "/packages/from-migration" Package.decoder body


putPackage : AppState -> { p | id : String, phase : PackagePhase } -> ToMsg () msg -> Cmd msg
putPackage appState package =
    let
        body =
            PackageDetail.encode package
    in
    Request.putWhatever (AppState.toServerInfo appState) ("/packages/" ++ package.id) body


deletePackage : AppState -> String -> String -> ToMsg () msg -> Cmd msg
deletePackage appState organizationId kmId =
    Request.delete (AppState.toServerInfo appState) ("/packages/?organizationId=" ++ organizationId ++ "&kmId=" ++ kmId)


deletePackageVersion : AppState -> String -> ToMsg () msg -> Cmd msg
deletePackageVersion appState packageId =
    Request.delete (AppState.toServerInfo appState) ("/packages/" ++ packageId)


pullPackage : AppState -> String -> ToMsg () msg -> Cmd msg
pullPackage appState packageId =
    Request.postEmpty (AppState.toServerInfo appState) ("/packages/" ++ packageId ++ "/pull")


importPackage : AppState -> File -> ToMsg () msg -> Cmd msg
importPackage appState file =
    Request.postFile (AppState.toServerInfo appState) "/packages/bundle" file


importFromOwl : AppState -> List ( String, String ) -> File -> ToMsg () msg -> Cmd msg
importFromOwl appState params file =
    let
        httpParams =
            List.map (\( k, v ) -> Http.stringPart k v) params
    in
    Request.postFileWithDataWhatever (AppState.toServerInfo appState) "/packages/bundle" file httpParams


exportPackageUrl : AppState -> String -> String
exportPackageUrl appState packageId =
    appState.apiUrl ++ "/packages/" ++ packageId ++ "/bundle"
