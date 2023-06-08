module Wizard.Projects.Create.CustomCreate.Msgs exposing (Msg(..))

import Form
import Shared.Data.KnowledgeModel exposing (KnowledgeModel)
import Shared.Data.PackageSuggestion exposing (PackageSuggestion)
import Shared.Data.Questionnaire exposing (Questionnaire)
import Shared.Error.ApiError exposing (ApiError)
import Wizard.Common.Components.TypeHintInput as TypeHintInput


type Msg
    = FormMsg Form.Msg
    | GetKnowledgeModelPreviewCompleted (Result ApiError KnowledgeModel)
    | AddTag String
    | RemoveTag String
    | ChangeUseAllQuestions Bool
    | PostQuestionnaireCompleted (Result ApiError Questionnaire)
    | PackageTypeHintInputMsg (TypeHintInput.Msg PackageSuggestion)
