module Shared.Api.Admin exposing
    ( executeOperation
    , getOperations
    )

import Json.Decode as D
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, jwtFetch, jwtGet)
import Shared.Data.AdminOperationExecution as AdminOperationExecution exposing (AdminOperationExecution)
import Shared.Data.AdminOperationExecutionResult as AdminOperationExecutionResult exposing (AdminOperationExecutionResult)
import Shared.Data.AdminOperationSection as AdminOperationSection exposing (AdminOperationSection)


getOperations : AbstractAppState a -> ToMsg (List AdminOperationSection) msg -> Cmd msg
getOperations =
    jwtGet "/admin/operations" (D.list AdminOperationSection.decoder)


executeOperation : AdminOperationExecution -> AbstractAppState a -> ToMsg AdminOperationExecutionResult msg -> Cmd msg
executeOperation execution =
    let
        body =
            AdminOperationExecution.encode execution
    in
    jwtFetch "/admin/operations/executions" AdminOperationExecutionResult.decoder body
