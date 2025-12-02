module Wizard.Api.KnowledgeModelPackages exposing
    ( deleteKnowledgeModelPackage
    , deleteKnowledgeModelPackageVersion
    , exportKnowledgeModelPackageUrl
    , getKnowledgeModelPackage
    , getKnowledgeModelPackageWithoutDeprecatedVersions
    , getKnowledgeModelPackages
    , getKnowledgeModelPackagesSuggestions
    , getKnowledgeModelPackagesSuggestionsWithOptions
    , getOutdatedKnowledgeModelPackages
    , importFromOwl
    , importKnowledgeModelPackage
    , postFromKnowledgeModelEditor
    , postFromMigration
    , pullKnowledgeModelPackage
    , putKnowledgeModelPackage
    )

import Common.Api.Models.Pagination as Pagination exposing (Pagination)
import Common.Api.Request as Request exposing (ToMsg)
import Common.Data.PaginationQueryFilters exposing (PaginationQueryFilters)
import Common.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Common.Utils.Bool as Bool
import File exposing (File)
import Http
import Json.Encode as E
import Maybe.Extra as Maybe
import Uuid exposing (Uuid)
import Wizard.Api.Models.KnowledgeModelPackage as KnowledgeModelPackage exposing (KnowledgeModelPackage)
import Wizard.Api.Models.KnowledgeModelPackage.KnowledgeModelPackagePhase as KnowledgeModelPackagePhase exposing (KnowledgeModelPackagePhase)
import Wizard.Api.Models.KnowledgeModelPackageDetail as KnowledgeModelPackageDetail exposing (KnowledgeModelPackageDetail)
import Wizard.Api.Models.KnowledgeModelPackageSuggestion as KnowledgeModelPackageSuggestion exposing (KnowledgeModelPackageSuggestion)
import Wizard.Data.AppState as AppState exposing (AppState)


getKnowledgeModelPackages : AppState -> PaginationQueryFilters -> PaginationQueryString -> ToMsg (Pagination KnowledgeModelPackage) msg -> Cmd msg
getKnowledgeModelPackages appState _ qs =
    let
        queryString =
            PaginationQueryString.toApiUrl qs

        url =
            "/knowledge-model-packages" ++ queryString
    in
    Request.get (AppState.toServerInfo appState) url (Pagination.decoder "knowledgeModelPackages" KnowledgeModelPackage.decoder)


getOutdatedKnowledgeModelPackages : AppState -> ToMsg (Pagination KnowledgeModelPackage) msg -> Cmd msg
getOutdatedKnowledgeModelPackages appState =
    let
        queryString =
            PaginationQueryString.empty
                |> PaginationQueryString.withSize (Just 5)
                |> PaginationQueryString.toApiUrlWith [ ( "outdated", "true" ) ]

        url =
            "/knowledge-model-packages" ++ queryString
    in
    Request.get (AppState.toServerInfo appState) url (Pagination.decoder "knowledgeModelPackages" KnowledgeModelPackage.decoder)


getKnowledgeModelPackagesSuggestions : AppState -> Maybe Bool -> PaginationQueryString -> ToMsg (Pagination KnowledgeModelPackageSuggestion) msg -> Cmd msg
getKnowledgeModelPackagesSuggestions appState nonEditable qs =
    let
        queryString =
            PaginationQueryString.toApiUrlWith
                [ ( "phase", KnowledgeModelPackagePhase.toString KnowledgeModelPackagePhase.Released )
                , ( "nonEditable", Maybe.unwrap "" Bool.toString nonEditable )
                ]
                qs

        url =
            "/knowledge-model-packages/suggestions" ++ queryString
    in
    Request.get (AppState.toServerInfo appState) url (Pagination.decoder "knowledgeModelPackages" KnowledgeModelPackageSuggestion.decoder)


getKnowledgeModelPackagesSuggestionsWithOptions : AppState -> PaginationQueryString -> List String -> List String -> ToMsg (Pagination KnowledgeModelPackageSuggestion) msg -> Cmd msg
getKnowledgeModelPackagesSuggestionsWithOptions appState qs select exclude =
    let
        queryString =
            PaginationQueryString.toApiUrlWith
                [ ( "select", String.join "," select )
                , ( "exclude", String.join "," exclude )
                ]
                qs

        url =
            "/knowledge-model-packages/suggestions" ++ queryString
    in
    Request.get (AppState.toServerInfo appState) url (Pagination.decoder "knowledgeModelPackages" KnowledgeModelPackageSuggestion.decoder)


getKnowledgeModelPackage : AppState -> String -> ToMsg KnowledgeModelPackageDetail msg -> Cmd msg
getKnowledgeModelPackage appState kmPackageId =
    Request.get (AppState.toServerInfo appState) ("/knowledge-model-packages/" ++ kmPackageId) KnowledgeModelPackageDetail.decoder


getKnowledgeModelPackageWithoutDeprecatedVersions : AppState -> String -> ToMsg KnowledgeModelPackageDetail msg -> Cmd msg
getKnowledgeModelPackageWithoutDeprecatedVersions appState kmPackageId =
    Request.get (AppState.toServerInfo appState) ("/knowledge-model-packages/" ++ kmPackageId ++ "?excludeDeprecatedVersions=true") KnowledgeModelPackageDetail.decoder


postFromKnowledgeModelEditor : AppState -> Uuid -> ToMsg KnowledgeModelPackage msg -> Cmd msg
postFromKnowledgeModelEditor appState uuid =
    let
        body =
            E.object [ ( "editorUuid", Uuid.encode uuid ) ]
    in
    Request.post (AppState.toServerInfo appState) "/knowledge-model-packages/from-editor" KnowledgeModelPackage.decoder body


postFromMigration : AppState -> E.Value -> ToMsg KnowledgeModelPackage msg -> Cmd msg
postFromMigration appState body =
    Request.post (AppState.toServerInfo appState) "/knowledge-model-packages/from-migration" KnowledgeModelPackage.decoder body


putKnowledgeModelPackage : AppState -> { p | id : String, phase : KnowledgeModelPackagePhase } -> ToMsg () msg -> Cmd msg
putKnowledgeModelPackage appState kmPackage =
    let
        body =
            KnowledgeModelPackageDetail.encode kmPackage
    in
    Request.putWhatever (AppState.toServerInfo appState) ("/knowledge-model-packages/" ++ kmPackage.id) body


deleteKnowledgeModelPackage : AppState -> String -> String -> ToMsg () msg -> Cmd msg
deleteKnowledgeModelPackage appState organizationId kmId =
    Request.delete (AppState.toServerInfo appState) ("/knowledge-model-packages/?organizationId=" ++ organizationId ++ "&kmId=" ++ kmId)


deleteKnowledgeModelPackageVersion : AppState -> String -> ToMsg () msg -> Cmd msg
deleteKnowledgeModelPackageVersion appState kmPackageId =
    Request.delete (AppState.toServerInfo appState) ("/knowledge-model-packages/" ++ kmPackageId)


pullKnowledgeModelPackage : AppState -> String -> ToMsg () msg -> Cmd msg
pullKnowledgeModelPackage appState kmPackageId =
    Request.postEmpty (AppState.toServerInfo appState) ("/knowledge-model-packages/" ++ kmPackageId ++ "/pull")


importKnowledgeModelPackage : AppState -> File -> ToMsg () msg -> Cmd msg
importKnowledgeModelPackage appState file =
    Request.postFile (AppState.toServerInfo appState) "/knowledge-model-packages/bundle" file


importFromOwl : AppState -> List ( String, String ) -> File -> ToMsg () msg -> Cmd msg
importFromOwl appState params file =
    let
        httpParams =
            List.map (\( k, v ) -> Http.stringPart k v) params
    in
    Request.postFileWithDataWhatever (AppState.toServerInfo appState) "/knowledge-model-packages/bundle" file httpParams


exportKnowledgeModelPackageUrl : String -> String
exportKnowledgeModelPackageUrl kmPackageId =
    "/knowledge-model-packages/" ++ kmPackageId ++ "/bundle"
