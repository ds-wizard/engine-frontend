module Wizard.Pages.KnowledgeModels.Import.OwlImport.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Form
import Json.Encode as E


type Msg
    = DragEnter
    | DragOver
    | DragLeave
    | FileSelected
    | FileRead E.Value
    | CancelFile
    | ImportOwlCompleted (Result ApiError ())
    | Cancel
    | FormMsg Form.Msg
