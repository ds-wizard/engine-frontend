module Wizard.Api.ProjectImporters exposing
    ( get
    , getList
    , getListFor
    , put
    )

import Common.Api.Models.Pagination as Pagination exposing (Pagination)
import Common.Api.Request as Request exposing (ToMsg)
import Common.Data.PaginationQueryFilters exposing (PaginationQueryFilters)
import Common.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Json.Decode as D
import Uuid exposing (Uuid)
import Wizard.Api.Models.ProjectImporter as ProjectImporter exposing (ProjectImporter)
import Wizard.Data.AppState as AppState exposing (AppState)


getList : AppState -> PaginationQueryFilters -> PaginationQueryString -> ToMsg (Pagination ProjectImporter) msg -> Cmd msg
getList appState _ qs =
    let
        queryString =
            PaginationQueryString.toApiUrl qs

        url =
            "/project-importers" ++ queryString
    in
    Request.get (AppState.toServerInfo appState) url (Pagination.decoder "projectImporters" ProjectImporter.decoder)


get : AppState -> String -> ToMsg ProjectImporter msg -> Cmd msg
get appState projectImporterId =
    Request.get (AppState.toServerInfo appState) ("/project-importers/" ++ projectImporterId) ProjectImporter.decoder


put : AppState -> ProjectImporter -> ToMsg () msg -> Cmd msg
put appState projectImporter =
    let
        body =
            ProjectImporter.encode projectImporter
    in
    Request.putWhatever (AppState.toServerInfo appState) ("/project-importers/" ++ projectImporter.id) body


getListFor : AppState -> Uuid -> ToMsg (List ProjectImporter) msg -> Cmd msg
getListFor appState projectUuid =
    let
        paginationQueryString =
            PaginationQueryString.empty
                |> PaginationQueryString.withSort (Just "name") PaginationQueryString.SortASC

        queryString =
            PaginationQueryString.toApiUrlWith
                [ ( "projectUuid", Uuid.toString projectUuid )
                , ( "enabled", "true" )
                ]
                paginationQueryString

        url =
            "/project-importers/suggestions" ++ queryString
    in
    Request.get (AppState.toServerInfo appState) url (D.map .items (Pagination.decoder "projectImporters" ProjectImporter.decoder))
