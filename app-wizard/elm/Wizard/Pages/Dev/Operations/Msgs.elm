module Wizard.Pages.Dev.Operations.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Common.Api.Models.DevOperationExecutionResult exposing (DevOperationExecutionResult)
import Common.Api.Models.DevOperationSection exposing (DevOperationSection)
import Common.Api.Models.TenantSuggestion exposing (TenantSuggestion)
import Common.Components.TypeHintInput as TypeHintInput


type Msg
    = GetDevOperationsComplete (Result ApiError (List DevOperationSection))
    | OpenSection String
    | FieldInput String String
    | FieldInputBool String Bool
    | UpdateTypeHintInput String (TypeHintInput.Msg TenantSuggestion)
    | ExecuteOperation String String
    | ExecuteOperationComplete String String (Result ApiError DevOperationExecutionResult)
