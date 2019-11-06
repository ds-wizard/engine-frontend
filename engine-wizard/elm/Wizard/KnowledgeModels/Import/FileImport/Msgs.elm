module Wizard.KnowledgeModels.Import.FileImport.Msgs exposing (Msg(..))

import Shared.Error.ApiError exposing (ApiError)
import Wizard.Ports as Ports exposing (FilePortData)


type Msg
    = DragEnter
    | DragOver
    | DragLeave
    | FileSelected
    | FileRead FilePortData
    | Submit
    | Cancel
    | ImportPackageCompleted (Result ApiError ())
