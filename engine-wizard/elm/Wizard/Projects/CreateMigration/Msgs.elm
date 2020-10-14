module Wizard.Projects.CreateMigration.Msgs exposing (Msg(..))

import Form
import Shared.Data.KnowledgeModel exposing (KnowledgeModel)
import Shared.Data.PackageSuggestion exposing (PackageSuggestion)
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Data.QuestionnaireMigration exposing (QuestionnaireMigration)
import Shared.Error.ApiError exposing (ApiError)
import Wizard.Common.Components.TypeHintInput as TypeHintInput


type Msg
    = GetQuestionnaireCompleted (Result ApiError QuestionnaireDetail)
    | FormMsg Form.Msg
    | SelectPackage PackageSuggestion
    | PostMigrationCompleted (Result ApiError QuestionnaireMigration)
    | GetKnowledgeModelPreviewCompleted (Result ApiError KnowledgeModel)
    | AddTag String
    | RemoveTag String
    | PackageTypeHintInputMsg (TypeHintInput.Msg PackageSuggestion)
