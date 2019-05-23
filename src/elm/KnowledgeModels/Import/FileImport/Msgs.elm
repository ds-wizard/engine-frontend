module KnowledgeModels.Import.FileImport.Msgs exposing (Msg(..))

import Common.ApiError exposing (ApiError)
import Ports exposing (FilePortData)


type Msg
    = DragEnter
    | DragOver
    | DragLeave
    | FileSelected
    | FileRead FilePortData
    | Submit
    | Cancel
    | ImportPackageCompleted (Result ApiError ())
