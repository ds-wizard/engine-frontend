module Common.Api.KnowledgeModelPackages exposing
    ( deleteKnowledgeModelPackage
    , deleteLocale
    , exportKnowledgeModelPackagePotUrl
    , exportKnowledgeModelPackageUrl
    , getKnowledgeModelPackage
    , getKnowledgeModelPackageDependents
    , getKnowledgeModelPackageWithoutDeprecatedVersions
    , getKnowledgeModelPackages
    , getKnowledgeModelPackagesSuggestions
    , getKnowledgeModelPackagesSuggestionsWithOptions
    , getLocaleContent
    , getOutdatedKnowledgeModelPackages
    , importKnowledgeModelPackage
    , importLocale
    , postFromKnowledgeModelEditor
    , postFromMigration
    , pullKnowledgeModelPackage
    , putKnowledgeModelPackage
    )

import Common.Api.Context as Context exposing (Context)
import Common.Api.Models.KnowledgeModelLocale as KnowledgeModelLocale exposing (KnowledgeModelLocale)
import Common.Api.Models.KnowledgeModelPackage as KnowledgeModelPackage exposing (KnowledgeModelPackage)
import Common.Api.Models.KnowledgeModelPackage.KnowledgeModelPackagePhase as KnowledgeModelPackagePhase exposing (KnowledgeModelPackagePhase)
import Common.Api.Models.KnowledgeModelPackageDetail as KnowledgeModelPackageDetail exposing (KnowledgeModelPackageDetail)
import Common.Api.Models.KnowledgeModelPackageSuggestion as KnowledgeModelPackageSuggestion exposing (KnowledgeModelPackageSuggestion)
import Common.Api.Models.LibraryDependents as LibraryDependents exposing (LibraryDependents)
import Common.Api.Models.Pagination as Pagination exposing (Pagination)
import Common.Api.Models.UuidResponse as UuidResponse exposing (UuidResponse)
import Common.Api.Params as Params
import Common.Api.Request as Request exposing (ToMsg)
import Common.Data.PaginationQueryFilters as PaginationQueryFilters exposing (PaginationQueryFilters)
import Common.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Common.Utils.Bool as Bool
import File exposing (File)
import Http
import Json.Encode as E
import Json.Encode.Extra as E
import Maybe.Extra as Maybe
import Uuid exposing (Uuid)


getKnowledgeModelPackages : Context ctx -> PaginationQueryFilters -> PaginationQueryString -> ToMsg (Pagination KnowledgeModelPackage) msg -> Cmd msg
getKnowledgeModelPackages appState pqf qs =
    let
        extraParams =
            createListExtraParams pqf

        queryString =
            PaginationQueryString.toApiUrlWith extraParams qs

        url =
            Params.apply appState "GET" ("/knowledge-model-packages" ++ queryString)
    in
    Request.get (Context.toServerInfo appState) url (Pagination.decoder "knowledgeModelPackages" KnowledgeModelPackage.decoder)


createListExtraParams : PaginationQueryFilters -> List ( String, String )
createListExtraParams filters =
    PaginationQueryString.filterParams
        [ ( "id", PaginationQueryFilters.getValue "id" filters ) ]


getOutdatedKnowledgeModelPackages : Context ctx -> ToMsg (Pagination KnowledgeModelPackage) msg -> Cmd msg
getOutdatedKnowledgeModelPackages appState =
    let
        queryString =
            PaginationQueryString.empty
                |> PaginationQueryString.withSize (Just 5)
                |> PaginationQueryString.toApiUrlWith [ ( "outdated", "true" ) ]

        url =
            Params.apply appState "GET" ("/knowledge-model-packages" ++ queryString)
    in
    Request.get (Context.toServerInfo appState) url (Pagination.decoder "knowledgeModelPackages" KnowledgeModelPackage.decoder)


getKnowledgeModelPackagesSuggestions : Context ctx -> Maybe Bool -> PaginationQueryString -> ToMsg (Pagination KnowledgeModelPackageSuggestion) msg -> Cmd msg
getKnowledgeModelPackagesSuggestions appState nonEditable qs =
    let
        queryString =
            PaginationQueryString.toApiUrlWith
                [ ( "phase", KnowledgeModelPackagePhase.toString KnowledgeModelPackagePhase.Released )
                , ( "nonEditable", Maybe.unwrap "" Bool.toString nonEditable )
                ]
                qs

        url =
            Params.apply appState "GET" ("/knowledge-model-packages/suggestions" ++ queryString)
    in
    Request.get (Context.toServerInfo appState) url (Pagination.decoder "knowledgeModelPackages" KnowledgeModelPackageSuggestion.decoder)


getKnowledgeModelPackagesSuggestionsWithOptions : Context ctx -> PaginationQueryString -> List String -> List String -> ToMsg (Pagination KnowledgeModelPackageSuggestion) msg -> Cmd msg
getKnowledgeModelPackagesSuggestionsWithOptions appState qs select exclude =
    let
        queryString =
            PaginationQueryString.toApiUrlWith
                [ ( "select", String.join "," select )
                , ( "exclude", String.join "," exclude )
                ]
                qs

        url =
            Params.apply appState "GET" ("/knowledge-model-packages/suggestions" ++ queryString)
    in
    Request.get (Context.toServerInfo appState) url (Pagination.decoder "knowledgeModelPackages" KnowledgeModelPackageSuggestion.decoder)


getKnowledgeModelPackage : Context ctx -> Uuid -> ToMsg KnowledgeModelPackageDetail msg -> Cmd msg
getKnowledgeModelPackage appState kmPackageUuid =
    Request.get (Context.toServerInfo appState) ("/knowledge-model-packages/" ++ Uuid.toString kmPackageUuid) KnowledgeModelPackageDetail.decoder


getKnowledgeModelPackageWithoutDeprecatedVersions : Context ctx -> Uuid -> ToMsg KnowledgeModelPackageDetail msg -> Cmd msg
getKnowledgeModelPackageWithoutDeprecatedVersions appState kmPackageUuid =
    Request.get (Context.toServerInfo appState) ("/knowledge-model-packages/" ++ Uuid.toString kmPackageUuid ++ "?excludeDeprecatedVersions=true") KnowledgeModelPackageDetail.decoder


getKnowledgeModelPackageDependents : Context ctx -> Uuid -> Bool -> ToMsg LibraryDependents msg -> Cmd msg
getKnowledgeModelPackageDependents appState kmPackageUuid allVersions =
    Request.get (Context.toServerInfo appState) ("/knowledge-model-packages/" ++ Uuid.toString kmPackageUuid ++ "/dependents?allVersions=" ++ Bool.toString allVersions) LibraryDependents.decoder


postFromKnowledgeModelEditor : Context ctx -> Uuid -> Maybe (List Uuid) -> ToMsg KnowledgeModelPackage msg -> Cmd msg
postFromKnowledgeModelEditor appState uuid mbLocaleUuids =
    let
        body =
            E.object
                [ ( "editorUuid", Uuid.encode uuid )
                , ( "localeUuids", E.maybe (E.list Uuid.encode) mbLocaleUuids )
                ]
    in
    Request.post (Context.toServerInfo appState) "/knowledge-model-packages/from-editor" KnowledgeModelPackage.decoder body


postFromMigration : Context ctx -> E.Value -> ToMsg KnowledgeModelPackage msg -> Cmd msg
postFromMigration appState body =
    Request.post (Context.toServerInfo appState) "/knowledge-model-packages/from-migration" KnowledgeModelPackage.decoder body


putKnowledgeModelPackage : Context ctx -> { p | uuid : Uuid, phase : KnowledgeModelPackagePhase, public : Bool } -> ToMsg () msg -> Cmd msg
putKnowledgeModelPackage appState kmPackage =
    let
        body =
            KnowledgeModelPackageDetail.encode kmPackage
    in
    Request.putWhatever (Context.toServerInfo appState) ("/knowledge-model-packages/" ++ Uuid.toString kmPackage.uuid) body


deleteKnowledgeModelPackage : Context ctx -> Uuid -> Bool -> ToMsg () msg -> Cmd msg
deleteKnowledgeModelPackage appState kmPackageUuid allVersions =
    Request.delete (Context.toServerInfo appState) ("/knowledge-model-packages/" ++ Uuid.toString kmPackageUuid ++ "?allVersions=" ++ Bool.toString allVersions)


pullKnowledgeModelPackage : Context ctx -> String -> ToMsg UuidResponse msg -> Cmd msg
pullKnowledgeModelPackage appState kmPackageId =
    Request.postEmptyBody (Context.toServerInfo appState) (Params.apply appState "POST" ("/knowledge-model-packages/" ++ kmPackageId ++ "/pull")) UuidResponse.decoder


importKnowledgeModelPackage : Context ctx -> File -> ToMsg () msg -> Cmd msg
importKnowledgeModelPackage appState file =
    Request.postFile (Context.toServerInfo appState) (Params.apply appState "POST" "/knowledge-model-packages/bundle") file


exportKnowledgeModelPackageUrl : Uuid -> String
exportKnowledgeModelPackageUrl kmPackageUuid =
    "/knowledge-model-packages/" ++ Uuid.toString kmPackageUuid ++ "/bundle"


exportKnowledgeModelPackagePotUrl : Uuid -> String
exportKnowledgeModelPackagePotUrl kmPackageUuid =
    "/knowledge-model-packages/" ++ Uuid.toString kmPackageUuid ++ "/locales/template"


deleteLocale : Context ctx -> Uuid -> Uuid -> ToMsg () msg -> Cmd msg
deleteLocale appState kmPackageUuid localeUuid =
    Request.delete (Context.toServerInfo appState) ("/knowledge-model-packages/" ++ Uuid.toString kmPackageUuid ++ "/locales/" ++ Uuid.toString localeUuid)


getLocaleContent : Context ctx -> Uuid -> Uuid -> ToMsg String msg -> Cmd msg
getLocaleContent appState kmPackageUuid localeUuid =
    Request.getString (Context.toServerInfo appState) ("/knowledge-model-packages/" ++ Uuid.toString kmPackageUuid ++ "/locales/" ++ Uuid.toString localeUuid ++ "/content")


importLocale : Context ctx -> Uuid -> String -> File -> File -> ToMsg KnowledgeModelLocale msg -> Cmd msg
importLocale appState kmPackageUuid name poContent jsonContent =
    let
        parts =
            [ Http.stringPart "name" name
            , Http.filePart "poContent" poContent
            , Http.filePart "jsonContent" jsonContent
            ]
    in
    Request.postMultiPartWithData (Context.toServerInfo appState) ("/knowledge-model-packages/" ++ Uuid.toString kmPackageUuid ++ "/locales") parts KnowledgeModelLocale.decoder
