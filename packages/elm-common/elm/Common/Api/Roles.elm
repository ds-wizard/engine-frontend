module Common.Api.Roles exposing (deleteRole, getRole, getRoles, postRole, putRole)

import Common.Api.Context as Context exposing (Context)
import Common.Api.Models.Pagination as Pagination exposing (Pagination)
import Common.Api.Models.Role as Role exposing (Role)
import Common.Api.Params as Params
import Common.Api.Request as Request exposing (ToMsg)
import Common.Data.PaginationQueryString as PaginationQueryString
import Json.Encode as E
import Uuid exposing (Uuid)


getRoles : Context ctx -> ToMsg (Pagination Role) msg -> Cmd msg
getRoles appState =
    let
        queryString =
            PaginationQueryString.empty
                |> PaginationQueryString.withSize (Just 100)
                |> PaginationQueryString.toApiUrl

        url =
            Params.apply appState "GET" ("/roles" ++ queryString)
    in
    Request.get (Context.toServerInfo appState) url (Pagination.decoder "roles" Role.decoder)


getRole : Context ctx -> Uuid -> ToMsg Role msg -> Cmd msg
getRole appState roleUuid =
    let
        url =
            "/roles/" ++ Uuid.toString roleUuid
    in
    Request.get (Context.toServerInfo appState) url Role.decoder


deleteRole : Context ctx -> Uuid -> ToMsg () msg -> Cmd msg
deleteRole appState roleUuid =
    let
        url =
            "/roles/" ++ Uuid.toString roleUuid
    in
    Request.delete (Context.toServerInfo appState) url


postRole : Context ctx -> E.Value -> ToMsg Role msg -> Cmd msg
postRole appState roleData =
    let
        url =
            Params.apply appState "POST" "/roles"
    in
    Request.post (Context.toServerInfo appState) url Role.decoder roleData


putRole : Context ctx -> Uuid -> E.Value -> ToMsg () msg -> Cmd msg
putRole appState roleUuid roleData =
    let
        url =
            "/roles/" ++ Uuid.toString roleUuid
    in
    Request.putWhatever (Context.toServerInfo appState) url roleData
