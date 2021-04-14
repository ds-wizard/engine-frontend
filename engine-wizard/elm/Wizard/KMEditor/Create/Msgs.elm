module Wizard.KMEditor.Create.Msgs exposing (Msg(..))

import Form
import Shared.Data.Branch exposing (Branch)
import Shared.Data.PackageDetail exposing (PackageDetail)
import Shared.Data.PackageSuggestion exposing (PackageSuggestion)
import Shared.Error.ApiError exposing (ApiError)
import Wizard.Common.Components.TypeHintInput as TypeHintInput


type Msg
    = FormMsg Form.Msg
    | PostBranchCompleted (Result ApiError Branch)
    | PackageTypeHintInputMsg (TypeHintInput.Msg PackageSuggestion)
    | GetPackageCompleted (Result ApiError PackageDetail)
