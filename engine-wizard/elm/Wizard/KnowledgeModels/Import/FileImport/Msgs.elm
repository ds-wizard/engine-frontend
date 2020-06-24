module Wizard.KnowledgeModels.Import.FileImport.Msgs exposing (Msg(..))

import Shared.Data.FilePortData exposing (FilePortData)
import Shared.Error.ApiError exposing (ApiError)


type Msg
    = DragEnter
    | DragOver
    | DragLeave
    | FileSelected
    | FileRead FilePortData
    | Submit
    | Cancel
    | ImportPackageCompleted (Result ApiError ())
