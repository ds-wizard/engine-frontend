module Wizard.Dev.Operations.Msgs exposing (Msg(..))

import Shared.Data.DevOperationExecutionResult exposing (DevOperationExecutionResult)
import Shared.Data.DevOperationSection exposing (DevOperationSection)
import Shared.Error.ApiError exposing (ApiError)


type Msg
    = GetAdminOperationsComplete (Result ApiError (List DevOperationSection))
    | OpenSection String
    | FieldInput String String
    | ExecuteOperation String String
    | ExecuteOperationComplete String String (Result ApiError DevOperationExecutionResult)
