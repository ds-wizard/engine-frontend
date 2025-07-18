module Wizard.KMEditor.Create.Msgs exposing (Msg(..))

import Form
import Shared.Data.ApiError exposing (ApiError)
import Version exposing (Version)
import Wizard.Api.Models.Branch exposing (Branch)
import Wizard.Api.Models.PackageDetail exposing (PackageDetail)
import Wizard.Api.Models.PackageSuggestion exposing (PackageSuggestion)
import Wizard.Common.Components.TypeHintInput as TypeHintInput


type Msg
    = Cancel
    | FormMsg Form.Msg
    | FormSetVersion Version
    | PostBranchCompleted (Result ApiError Branch)
    | PackageTypeHintInputMsg (TypeHintInput.Msg PackageSuggestion)
    | GetPackageCompleted (Result ApiError PackageDetail)
