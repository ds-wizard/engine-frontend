module Wizard.Api.Roles exposing (deleteRole, getRole, getRoles, postRole, putRole)

import Common.Api.Models.Pagination as Pagination exposing (Pagination)
import Common.Api.Models.Role as Role exposing (Role)
import Common.Api.Request as Request exposing (ToMsg)
import Common.Data.PaginationQueryString as PaginationQueryString
import Json.Encode as E
import Uuid exposing (Uuid)
import Wizard.Data.AppState as AppState exposing (AppState)


getRoles : AppState -> ToMsg (Pagination Role) msg -> Cmd msg
getRoles appState =
    let
        queryString =
            PaginationQueryString.empty
                |> PaginationQueryString.withSize (Just 100)
                |> PaginationQueryString.toApiUrl

        url =
            "/roles" ++ queryString
    in
    Request.get (AppState.toServerInfo appState) url (Pagination.decoder "roles" Role.decoder)


getRole : AppState -> Uuid -> ToMsg Role msg -> Cmd msg
getRole appState roleUuid =
    let
        url =
            "/roles/" ++ Uuid.toString roleUuid
    in
    Request.get (AppState.toServerInfo appState) url Role.decoder


deleteRole : AppState -> Uuid -> ToMsg () msg -> Cmd msg
deleteRole appState roleUuid =
    let
        url =
            "/roles/" ++ Uuid.toString roleUuid
    in
    Request.delete (AppState.toServerInfo appState) url


postRole : AppState -> E.Value -> ToMsg Role msg -> Cmd msg
postRole appState roleData =
    let
        url =
            "/roles"
    in
    Request.post (AppState.toServerInfo appState) url Role.decoder roleData


putRole : AppState -> Uuid -> E.Value -> ToMsg () msg -> Cmd msg
putRole appState roleUuid roleData =
    let
        url =
            "/roles/" ++ Uuid.toString roleUuid
    in
    Request.putWhatever (AppState.toServerInfo appState) url roleData
