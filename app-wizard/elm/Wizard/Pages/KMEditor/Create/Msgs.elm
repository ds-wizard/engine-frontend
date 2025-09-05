module Wizard.Pages.KMEditor.Create.Msgs exposing (Msg(..))

import Common.Data.ApiError exposing (ApiError)
import Form
import Version exposing (Version)
import Wizard.Api.Models.Branch exposing (Branch)
import Wizard.Api.Models.PackageDetail exposing (PackageDetail)
import Wizard.Api.Models.PackageSuggestion exposing (PackageSuggestion)
import Wizard.Components.TypeHintInput as TypeHintInput


type Msg
    = Cancel
    | FormMsg Form.Msg
    | FormSetVersion Version
    | PostBranchCompleted (Result ApiError Branch)
    | PackageTypeHintInputMsg (TypeHintInput.Msg PackageSuggestion)
    | GetPackageCompleted (Result ApiError PackageDetail)
