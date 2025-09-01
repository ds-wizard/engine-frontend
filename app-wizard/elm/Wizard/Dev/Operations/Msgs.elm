module Wizard.Dev.Operations.Msgs exposing (Msg(..))

import Shared.Data.ApiError exposing (ApiError)
import Shared.Data.DevOperationExecutionResult exposing (DevOperationExecutionResult)
import Shared.Data.DevOperationSection exposing (DevOperationSection)


type Msg
    = GetDevOperationsComplete (Result ApiError (List DevOperationSection))
    | OpenSection String
    | FieldInput String String
    | ExecuteOperation String String
    | ExecuteOperationComplete String String (Result ApiError DevOperationExecutionResult)
