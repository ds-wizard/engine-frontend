module Wizard.Pages.KMEditor.Create.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Common.Api.Models.UuidResponse exposing (UuidResponse)
import Common.Components.TypeHintInput as TypeHintInput
import Form
import Version exposing (Version)
import Wizard.Api.Models.KnowledgeModelPackageDetail exposing (KnowledgeModelPackageDetail)
import Wizard.Api.Models.KnowledgeModelPackageSuggestion exposing (KnowledgeModelPackageSuggestion)


type Msg
    = Cancel
    | FormMsg Form.Msg
    | FormSetVersion Version
    | PostKmEditorCompleted (Result ApiError UuidResponse)
    | KnowledgeModelPackageTypeHintInputMsg (TypeHintInput.Msg KnowledgeModelPackageSuggestion)
    | GetPackageCompleted (Result ApiError KnowledgeModelPackageDetail)
