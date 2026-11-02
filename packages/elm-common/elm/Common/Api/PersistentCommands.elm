module Common.Api.PersistentCommands exposing
    ( getPersistentCommand
    , getPersistentCommands
    , retry
    , retryAllFailed
    , updateState
    )

import Common.Api.Context as Context exposing (Context)
import Common.Api.Models.Pagination as Pagination exposing (Pagination)
import Common.Api.Models.PersistentCommand as PersistentCommand exposing (PersistentCommand)
import Common.Api.Models.PersistentCommand.PersistentCommandState as PersistentCommandState exposing (PersistentCommandState)
import Common.Api.Models.PersistentCommandDetail as PersistentCommandDetail exposing (PersistentCommandDetail)
import Common.Api.Request as Request exposing (ToMsg)
import Common.Data.PaginationQueryFilters as PaginationQueryFilters exposing (PaginationQueryFilters)
import Common.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Json.Encode as E
import Uuid exposing (Uuid)


getPersistentCommands : Context ctx -> PaginationQueryFilters -> PaginationQueryString -> ToMsg (Pagination PersistentCommand) msg -> Cmd msg
getPersistentCommands appState filters qs =
    let
        extraParams =
            PaginationQueryString.filterParams
                [ ( "state", PaginationQueryFilters.getValue "state" filters ) ]

        queryString =
            PaginationQueryString.toApiUrlWith extraParams qs

        url =
            "/persistent-commands" ++ queryString
    in
    Request.get (Context.toServerInfo appState) url (Pagination.decoder "persistentCommands" PersistentCommand.decoder)


getPersistentCommand : Context ctx -> Uuid -> ToMsg PersistentCommandDetail msg -> Cmd msg
getPersistentCommand appState uuid =
    Request.get (Context.toServerInfo appState) ("/persistent-commands/" ++ Uuid.toString uuid) PersistentCommandDetail.decoder


updateState : Context ctx -> Uuid -> PersistentCommandState -> ToMsg () msg -> Cmd msg
updateState appState uuid newState =
    let
        body =
            E.object [ ( "state", PersistentCommandState.encode newState ) ]
    in
    Request.putWhatever (Context.toServerInfo appState) ("/persistent-commands/" ++ Uuid.toString uuid) body


retry : Context ctx -> Uuid -> ToMsg () msg -> Cmd msg
retry appState uuid =
    Request.postEmpty (Context.toServerInfo appState) ("/persistent-commands/" ++ Uuid.toString uuid ++ "/attempts")


retryAllFailed : Context ctx -> ToMsg () msg -> Cmd msg
retryAllFailed appState =
    Request.postEmpty (Context.toServerInfo appState) "/persistent-commands/attempts"
