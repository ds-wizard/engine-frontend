module Shared.Api.DevOperations exposing
    ( executeOperation
    , getOperations
    )

import Json.Decode as D
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, jwtFetch, jwtGet)
import Shared.Data.DevOperationExecution as AdminOperationExecution exposing (DevOperationExecution)
import Shared.Data.DevOperationExecutionResult as AdminOperationExecutionResult exposing (DevOperationExecutionResult)
import Shared.Data.DevOperationSection as AdminOperationSection exposing (DevOperationSection)


getOperations : AbstractAppState a -> ToMsg (List DevOperationSection) msg -> Cmd msg
getOperations =
    jwtGet "/dev-operations" (D.list AdminOperationSection.decoder)


executeOperation : DevOperationExecution -> AbstractAppState a -> ToMsg DevOperationExecutionResult msg -> Cmd msg
executeOperation execution =
    let
        body =
            AdminOperationExecution.encode execution
    in
    jwtFetch "/dev-operations/executions" AdminOperationExecutionResult.decoder body
