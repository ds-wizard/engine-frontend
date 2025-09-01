module Wizard.Api.PersistentCommands exposing
    ( getPersistentCommand
    , getPersistentCommands
    , retry
    , retryAllFailed
    , updateState
    )

import Json.Encode as E
import Shared.Api.Request as Request exposing (ToMsg)
import Shared.Data.Pagination as Pagination exposing (Pagination)
import Shared.Data.PaginationQueryFilters as PaginationQueryFilters exposing (PaginationQueryFilters)
import Shared.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Shared.Data.PersistentCommandState as PersistentCommandState exposing (PersistentCommandState)
import Uuid exposing (Uuid)
import Wizard.Api.Models.PersistentCommand as PersistentCommand exposing (PersistentCommand)
import Wizard.Api.Models.PersistentCommandDetail as PersistentCommandDetail exposing (PersistentCommandDetail)
import Wizard.Data.AppState as AppState exposing (AppState)


getPersistentCommands : AppState -> PaginationQueryFilters -> PaginationQueryString -> ToMsg (Pagination PersistentCommand) msg -> Cmd msg
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
    Request.get (AppState.toServerInfo appState) url (Pagination.decoder "persistentCommands" PersistentCommand.decoder)


getPersistentCommand : AppState -> Uuid -> ToMsg PersistentCommandDetail msg -> Cmd msg
getPersistentCommand appState uuid =
    Request.get (AppState.toServerInfo appState) ("/persistent-commands/" ++ Uuid.toString uuid) PersistentCommandDetail.decoder


updateState : AppState -> Uuid -> PersistentCommandState -> ToMsg () msg -> Cmd msg
updateState appState uuid newState =
    let
        body =
            E.object [ ( "state", PersistentCommandState.encode newState ) ]
    in
    Request.putWhatever (AppState.toServerInfo appState) ("/persistent-commands/" ++ Uuid.toString uuid) body


retry : AppState -> Uuid -> ToMsg () msg -> Cmd msg
retry appState uuid =
    Request.postEmpty (AppState.toServerInfo appState) ("/persistent-commands/" ++ Uuid.toString uuid ++ "/attempts")


retryAllFailed : AppState -> ToMsg () msg -> Cmd msg
retryAllFailed appState =
    Request.postEmpty (AppState.toServerInfo appState) "/persistent-commands/attempts"
