module Wizard.Pages.Dev.Operations.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Common.Api.Models.DevOperationExecutionResult exposing (DevOperationExecutionResult)
import Common.Api.Models.DevOperationSection exposing (DevOperationSection)


type Msg
    = GetDevOperationsComplete (Result ApiError (List DevOperationSection))
    | OpenSection String
    | FieldInput String String
    | ExecuteOperation String String
    | ExecuteOperationComplete String String (Result ApiError DevOperationExecutionResult)
