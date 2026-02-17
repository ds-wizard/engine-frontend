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
import Common.Api.Models.UuidResponse as UuidResponse exposing (UuidResponse)
import Common.Api.Request as Request exposing (ToMsg)
import Common.Data.PaginationQueryFilters as PaginationQueryFilters exposing (PaginationQueryFilters)
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
getKnowledgeModelPackages appState pqf qs =
    let
        extraParams =
            createListExtraParams pqf

        queryString =
            PaginationQueryString.toApiUrlWith extraParams qs

        url =
            "/knowledge-model-packages" ++ queryString
    in
    Request.get (AppState.toServerInfo appState) url (Pagination.decoder "knowledgeModelPackages" KnowledgeModelPackage.decoder)


createListExtraParams : PaginationQueryFilters -> List ( String, String )
createListExtraParams filters =
    PaginationQueryString.filterParams
        [ ( "organizationId", PaginationQueryFilters.getValue "organizationId" filters )
        , ( "kmId", PaginationQueryFilters.getValue "kmId" filters )
        ]


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


getKnowledgeModelPackage : AppState -> Uuid -> ToMsg KnowledgeModelPackageDetail msg -> Cmd msg
getKnowledgeModelPackage appState kmPackageUuid =
    Request.get (AppState.toServerInfo appState) ("/knowledge-model-packages/" ++ Uuid.toString kmPackageUuid) KnowledgeModelPackageDetail.decoder


getKnowledgeModelPackageWithoutDeprecatedVersions : AppState -> Uuid -> ToMsg KnowledgeModelPackageDetail msg -> Cmd msg
getKnowledgeModelPackageWithoutDeprecatedVersions appState kmPackageUuid =
    Request.get (AppState.toServerInfo appState) ("/knowledge-model-packages/" ++ Uuid.toString kmPackageUuid ++ "?excludeDeprecatedVersions=true") KnowledgeModelPackageDetail.decoder


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


putKnowledgeModelPackage : AppState -> { p | uuid : Uuid, phase : KnowledgeModelPackagePhase, public : Bool } -> ToMsg () msg -> Cmd msg
putKnowledgeModelPackage appState kmPackage =
    let
        body =
            KnowledgeModelPackageDetail.encode kmPackage
    in
    Request.putWhatever (AppState.toServerInfo appState) ("/knowledge-model-packages/" ++ Uuid.toString kmPackage.uuid) body


deleteKnowledgeModelPackage : AppState -> String -> String -> ToMsg () msg -> Cmd msg
deleteKnowledgeModelPackage appState organizationId kmId =
    Request.delete (AppState.toServerInfo appState) ("/knowledge-model-packages/?organizationId=" ++ organizationId ++ "&kmId=" ++ kmId)


deleteKnowledgeModelPackageVersion : AppState -> Uuid -> ToMsg () msg -> Cmd msg
deleteKnowledgeModelPackageVersion appState kmPackageUuid =
    Request.delete (AppState.toServerInfo appState) ("/knowledge-model-packages/" ++ Uuid.toString kmPackageUuid)


pullKnowledgeModelPackage : AppState -> String -> ToMsg UuidResponse msg -> Cmd msg
pullKnowledgeModelPackage appState kmPackageId =
    Request.postEmptyBody (AppState.toServerInfo appState) ("/knowledge-model-packages/" ++ kmPackageId ++ "/pull") UuidResponse.decoder


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


exportKnowledgeModelPackageUrl : Uuid -> String
exportKnowledgeModelPackageUrl kmPackageUuid =
    "/knowledge-model-packages/" ++ Uuid.toString kmPackageUuid ++ "/bundle"
