module Wizard.Pages.Projects.CreateMigration.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Common.Components.TypeHintInput as TypeHintInput
import Form
import Wizard.Api.Models.KnowledgeModel exposing (KnowledgeModel)
import Wizard.Api.Models.KnowledgeModelPackageDetail exposing (KnowledgeModelPackageDetail)
import Wizard.Api.Models.KnowledgeModelPackageSuggestion exposing (KnowledgeModelPackageSuggestion)
import Wizard.Api.Models.QuestionnaireDetailWrapper exposing (QuestionnaireDetailWrapper)
import Wizard.Api.Models.QuestionnaireMigration exposing (QuestionnaireMigration)
import Wizard.Api.Models.QuestionnaireSettings exposing (QuestionnaireSettings)


type Msg
    = GetQuestionnaireCompleted (Result ApiError (QuestionnaireDetailWrapper QuestionnaireSettings))
    | Cancel
    | FormMsg Form.Msg
    | SelectKnowledgeModelPackage KnowledgeModelPackageSuggestion
    | PostMigrationCompleted (Result ApiError QuestionnaireMigration)
    | GetKnowledgeModelPreviewCompleted (Result ApiError KnowledgeModel)
    | GetCurrentKnowledgeModelPackageCompleted (Result ApiError KnowledgeModelPackageDetail)
    | GetSelectedKnowledgeModelPackageCompleted (Result ApiError KnowledgeModelPackageDetail)
    | AddTag String
    | RemoveTag String
    | ChangeUseAllQuestions Bool
    | KnowledgeModelPackageTypeHintInputMsg (TypeHintInput.Msg KnowledgeModelPackageSuggestion)
