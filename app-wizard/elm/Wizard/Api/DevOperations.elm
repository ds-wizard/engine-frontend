module Wizard.Api.DevOperations exposing
    ( executeOperation
    , getOperations
    )

import Common.Api.Request as Request exposing (ToMsg)
import Common.Data.DevOperationExecution as DevOperationExecution exposing (DevOperationExecution)
import Common.Data.DevOperationExecutionResult as DevOperationExecutionResult exposing (DevOperationExecutionResult)
import Common.Data.DevOperationSection as DevOperationSection exposing (DevOperationSection)
import Json.Decode as D
import Wizard.Data.AppState as AppState exposing (AppState)


getOperations : AppState -> ToMsg (List DevOperationSection) msg -> Cmd msg
getOperations appState =
    Request.get (AppState.toServerInfo appState) "/dev-operations" (D.list DevOperationSection.decoder)


executeOperation : AppState -> DevOperationExecution -> ToMsg DevOperationExecutionResult msg -> Cmd msg
executeOperation appState execution =
    let
        body =
            DevOperationExecution.encode execution
    in
    Request.post (AppState.toServerInfo appState) "/dev-operations/executions" DevOperationExecutionResult.decoder body
