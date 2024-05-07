module Wizard.KMEditor.Create.Msgs exposing (Msg(..))

import Form
import Shared.Data.Branch exposing (Branch)
import Shared.Data.PackageDetail exposing (PackageDetail)
import Shared.Data.PackageSuggestion exposing (PackageSuggestion)
import Shared.Error.ApiError exposing (ApiError)
import Version exposing (Version)
import Wizard.Common.Components.TypeHintInput as TypeHintInput


type Msg
    = Cancel
    | FormMsg Form.Msg
    | FormSetVersion Version
    | PostBranchCompleted (Result ApiError Branch)
    | PackageTypeHintInputMsg (TypeHintInput.Msg PackageSuggestion)
    | GetPackageCompleted (Result ApiError PackageDetail)
