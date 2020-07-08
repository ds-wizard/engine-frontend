module Wizard.Templates.Import.FileImport.Msgs exposing (Msg(..))

import Json.Encode as E
import Shared.Error.ApiError exposing (ApiError)


type Msg
    = DragEnter
    | DragOver
    | DragLeave
    | FileSelected
    | FileRead E.Value
    | Submit
    | Cancel
    | ImportTemplateCompleted (Result ApiError ())
