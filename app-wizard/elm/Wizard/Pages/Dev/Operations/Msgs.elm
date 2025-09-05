module Wizard.Pages.Dev.Operations.Msgs exposing (Msg(..))

import Common.Data.ApiError exposing (ApiError)
import Common.Data.DevOperationExecutionResult exposing (DevOperationExecutionResult)
import Common.Data.DevOperationSection exposing (DevOperationSection)


type Msg
    = GetDevOperationsComplete (Result ApiError (List DevOperationSection))
    | OpenSection String
    | FieldInput String String
    | ExecuteOperation String String
    | ExecuteOperationComplete String String (Result ApiError DevOperationExecutionResult)
