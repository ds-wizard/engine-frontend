module Wizard.Api.ProjectActions exposing
    ( getList
    , getListFor
    , put
    )

import Common.Api.Models.Pagination as Pagination exposing (Pagination)
import Common.Api.Request as Request exposing (ToMsg)
import Common.Data.PaginationQueryFilters exposing (PaginationQueryFilters)
import Common.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Json.Decode as D
import Uuid exposing (Uuid)
import Wizard.Api.Models.ProjectAction as ProjectAction exposing (ProjectAction)
import Wizard.Data.AppState as AppState exposing (AppState)


getList : AppState -> PaginationQueryFilters -> PaginationQueryString -> ToMsg (Pagination ProjectAction) msg -> Cmd msg
getList appState _ qs =
    let
        queryString =
            PaginationQueryString.toApiUrl qs

        url =
            "/project-actions" ++ queryString
    in
    Request.get (AppState.toServerInfo appState) url (Pagination.decoder "projectActions" ProjectAction.decoder)


put : AppState -> ProjectAction -> ToMsg () msg -> Cmd msg
put appState projectAction =
    let
        body =
            ProjectAction.encode projectAction
    in
    Request.putWhatever (AppState.toServerInfo appState) ("/project-actions/" ++ projectAction.id) body


getListFor : AppState -> Uuid -> ToMsg (List ProjectAction) msg -> Cmd msg
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
            "/project-actions/suggestions" ++ queryString
    in
    Request.get (AppState.toServerInfo appState) url (D.map .items (Pagination.decoder "projectActions" ProjectAction.decoder))
