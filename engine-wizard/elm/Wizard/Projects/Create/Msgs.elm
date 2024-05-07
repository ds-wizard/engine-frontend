module Wizard.Projects.Create.Msgs exposing (Msg(..))

import Form
import Shared.Data.KnowledgeModel exposing (KnowledgeModel)
import Shared.Data.PackageDetail exposing (PackageDetail)
import Shared.Data.PackageSuggestion exposing (PackageSuggestion)
import Shared.Data.Pagination exposing (Pagination)
import Shared.Data.Questionnaire exposing (Questionnaire)
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Error.ApiError exposing (ApiError)
import Wizard.Common.Components.TypeHintInput as TypeHintInput
import Wizard.Projects.Create.Models exposing (ActiveTab)


type Msg
    = GetSelectedProjectTemplateCompleted (Result ApiError QuestionnaireDetail)
    | GetSelectedKnowledgeModelCompleted (Result ApiError PackageDetail)
    | GetProjectTemplatesCountCompleted (Result ApiError (Pagination Questionnaire))
    | GetKnowledgeModelsCountCompleted (Result ApiError (Pagination PackageSuggestion))
    | Cancel
    | FormMsg Form.Msg
    | PostQuestionnaireCompleted (Result ApiError Questionnaire)
    | GetKnowledgeModelPreviewCompleted (Result ApiError KnowledgeModel)
    | AddTag String
    | RemoveTag String
    | ChangeUseAllQuestions Bool
    | SetActiveTab ActiveTab
    | ProjectTemplateTypeHintInputMsg (TypeHintInput.Msg Questionnaire)
    | KnowledgeModelTypeHintInputMsg (TypeHintInput.Msg PackageSuggestion)
