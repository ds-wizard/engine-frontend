module Wizard.Pages.KnowledgeModels.Compare.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Wizard.Api.Models.KnowledgeModelPackageDetail exposing (KnowledgeModelPackageDetail)
import Wizard.Components.KMComparison as KMComparison
import Wizard.Pages.KnowledgeModels.Common.CompareSelectModal as CompareSelectModal


type Msg
    = GetInitialKnowledgeModelCompleted (Result ApiError KnowledgeModelPackageDetail)
    | KMComparisonMsg KMComparison.Msg
    | CompareSelectModalMsg CompareSelectModal.Msg
