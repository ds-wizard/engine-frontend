module Wizard.Admin.Operations.Msgs exposing (Msg(..))

import Shared.Data.AdminOperationExecutionResult exposing (AdminOperationExecutionResult)
import Shared.Data.AdminOperationSection exposing (AdminOperationSection)
import Shared.Error.ApiError exposing (ApiError)


type Msg
    = GetAdminOperationsComplete (Result ApiError (List AdminOperationSection))
    | OpenSection String
    | FieldInput String String
    | ExecuteOperation String String
    | ExecuteOperationComplete String String (Result ApiError AdminOperationExecutionResult)
