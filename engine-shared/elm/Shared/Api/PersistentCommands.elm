module Shared.Api.PersistentCommands exposing (GetPersistentCommandsFilters, getPersistentCommand, getPersistentCommands, retry, retryAllFailed)

import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, jwtGet, jwtPostEmpty)
import Shared.Data.Pagination as Pagination exposing (Pagination)
import Shared.Data.PaginationQueryString as PaginationQueryString exposing (PaginationQueryString)
import Shared.Data.PersistentCommand as PersistentCommand exposing (PersistentCommand)
import Shared.Data.PersistentCommandDetail as PersistentCommandDetail exposing (PersistentCommandDetail)
import Uuid exposing (Uuid)


type alias GetPersistentCommandsFilters =
    { state : Maybe String }


getPersistentCommands : GetPersistentCommandsFilters -> PaginationQueryString -> AbstractAppState a -> ToMsg (Pagination PersistentCommand) msg -> Cmd msg
getPersistentCommands filters qs =
    let
        extraParams =
            PaginationQueryString.filterParams <|
                [ ( "state", filters.state ) ]

        queryString =
            PaginationQueryString.toApiUrlWith extraParams qs

        url =
            "/persistent-commands" ++ queryString
    in
    jwtGet url (Pagination.decoder "persistentCommands" PersistentCommand.decoder)


getPersistentCommand : Uuid -> AbstractAppState a -> ToMsg PersistentCommandDetail msg -> Cmd msg
getPersistentCommand uuid =
    jwtGet ("/persistent-commands/" ++ Uuid.toString uuid) PersistentCommandDetail.decoder


retry : Uuid -> AbstractAppState a -> ToMsg () msg -> Cmd msg
retry uuid =
    jwtPostEmpty ("/persistent-commands/" ++ Uuid.toString uuid ++ "/attempts")


retryAllFailed : AbstractAppState a -> ToMsg () msg -> Cmd msg
retryAllFailed =
    jwtPostEmpty "/persistent-commands/attempts"
