module Wizard.Pages.KMEditor.Create.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Common.Components.TypeHintInput as TypeHintInput
import Form
import Version exposing (Version)
import Wizard.Api.Models.KnowledgeModelEditor exposing (KnowledgeModelEditor)
import Wizard.Api.Models.PackageDetail exposing (PackageDetail)
import Wizard.Api.Models.PackageSuggestion exposing (PackageSuggestion)


type Msg
    = Cancel
    | FormMsg Form.Msg
    | FormSetVersion Version
    | PostKmEditorCompleted (Result ApiError KnowledgeModelEditor)
    | PackageTypeHintInputMsg (TypeHintInput.Msg PackageSuggestion)
    | GetPackageCompleted (Result ApiError PackageDetail)
