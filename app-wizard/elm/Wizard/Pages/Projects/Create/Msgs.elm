module Wizard.Pages.Projects.Create.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Common.Api.Models.Pagination exposing (Pagination)
import Common.Components.TypeHintInput as TypeHintInput
import Form
import Wizard.Api.Models.KnowledgeModel exposing (KnowledgeModel)
import Wizard.Api.Models.KnowledgeModelPackageDetail exposing (KnowledgeModelPackageDetail)
import Wizard.Api.Models.KnowledgeModelPackageSuggestion exposing (KnowledgeModelPackageSuggestion)
import Wizard.Api.Models.Project exposing (Project)
import Wizard.Api.Models.ProjectDetailWrapper exposing (ProjectDetailWrapper)
import Wizard.Api.Models.ProjectSettings exposing (ProjectSettings)
import Wizard.Pages.Projects.Create.Models exposing (ActiveTab)


type Msg
    = GetSelectedProjectTemplateCompleted (Result ApiError (ProjectDetailWrapper ProjectSettings))
    | GetSelectedKnowledgeModelCompleted (Result ApiError KnowledgeModelPackageDetail)
    | GetProjectTemplatesCountCompleted (Result ApiError (Pagination Project))
    | GetKnowledgeModelsCountCompleted (Result ApiError (Pagination KnowledgeModelPackageSuggestion))
    | Cancel
    | FormMsg Form.Msg
    | PostProjectCompleted (Result ApiError Project)
    | GetKnowledgeModelPreviewCompleted (Result ApiError KnowledgeModel)
    | AddTag String
    | RemoveTag String
    | ChangeUseAllQuestions Bool
    | SetActiveTab ActiveTab
    | ProjectTemplateTypeHintInputMsg (TypeHintInput.Msg Project)
    | KnowledgeModelTypeHintInputMsg (TypeHintInput.Msg KnowledgeModelPackageSuggestion)
