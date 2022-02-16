module Wizard.KnowledgeModels.Import.OwlImport.Msgs exposing (Msg(..))

import Form
import Json.Encode as E
import Shared.Error.ApiError exposing (ApiError)


type Msg
    = DragEnter
    | DragOver
    | DragLeave
    | FileSelected
    | FileRead E.Value
    | CancelFile
    | ImportOwlCompleted (Result ApiError ())
    | FormMsg Form.Msg
