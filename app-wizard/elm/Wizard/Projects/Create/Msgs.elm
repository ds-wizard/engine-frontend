module Wizard.Projects.Create.Msgs exposing (Msg(..))

import Form
import Shared.Data.ApiError exposing (ApiError)
import Shared.Data.Pagination exposing (Pagination)
import Wizard.Api.Models.KnowledgeModel exposing (KnowledgeModel)
import Wizard.Api.Models.PackageDetail exposing (PackageDetail)
import Wizard.Api.Models.PackageSuggestion exposing (PackageSuggestion)
import Wizard.Api.Models.Questionnaire exposing (Questionnaire)
import Wizard.Api.Models.QuestionnaireDetailWrapper exposing (QuestionnaireDetailWrapper)
import Wizard.Api.Models.QuestionnaireSettings exposing (QuestionnaireSettings)
import Wizard.Common.Components.TypeHintInput as TypeHintInput
import Wizard.Projects.Create.Models exposing (ActiveTab)


type Msg
    = GetSelectedProjectTemplateCompleted (Result ApiError (QuestionnaireDetailWrapper QuestionnaireSettings))
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
