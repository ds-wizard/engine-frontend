module Wizard.Api.ProjectFiles exposing
    ( delete
    , fileUrl
    , getFileUrl
    , getList
    , post
    )

import Common.Api.Models.Pagination as Pagination exposing (Pagination)
import Common.Api.Models.UrlResponse as UrlResponse exposing (UrlResponse)
import Common.Api.Request as Request exposing (ToMsg)
import Common.Data.PaginationQueryFilters exposing (PaginationQueryFilters)
import Common.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import File exposing (File)
import Http
import Uuid exposing (Uuid)
import Wizard.Api.Models.ProjectFile as ProjectFile exposing (ProjectFile)
import Wizard.Api.Models.ProjectFileSimple as ProjectFileSimple exposing (ProjectFileSimple)
import Wizard.Data.AppState as AppState exposing (AppState)


getList : AppState -> PaginationQueryFilters -> PaginationQueryString -> ToMsg (Pagination ProjectFile) msg -> Cmd msg
getList appState _ qs =
    let
        queryString =
            PaginationQueryString.toApiUrl qs

        url =
            "/project-files" ++ queryString
    in
    Request.get (AppState.toServerInfo appState) url (Pagination.decoder "projectFiles" ProjectFile.decoder)


post : AppState -> Uuid -> String -> File -> ToMsg ProjectFileSimple msg -> Cmd msg
post appState projectUuid questionUuidString file =
    let
        extraParts =
            [ Http.stringPart "fileName" (File.name file) ]

        url =
            "/projects/" ++ Uuid.toString projectUuid ++ "/files/" ++ questionUuidString
    in
    Request.postFileWithData (AppState.toServerInfo appState) url file extraParts ProjectFileSimple.decoder


delete : AppState -> Uuid -> Uuid -> ToMsg () msg -> Cmd msg
delete appState projectUuid fileUuid =
    let
        url =
            "/projects/" ++ Uuid.toString projectUuid ++ "/files/" ++ Uuid.toString fileUuid
    in
    Request.delete (AppState.toServerInfo appState) url


getFileUrl : AppState -> Uuid -> Uuid -> ToMsg UrlResponse msg -> Cmd msg
getFileUrl appState projectUuid fileUuid =
    let
        url =
            "/projects/" ++ Uuid.toString projectUuid ++ "/files/" ++ Uuid.toString fileUuid
    in
    Request.get (AppState.toServerInfo appState) url UrlResponse.decoder


fileUrl : Uuid -> Uuid -> String
fileUrl projectUuid fileUuid =
    "/projects/" ++ Uuid.toString projectUuid ++ "/files/" ++ Uuid.toString fileUuid
