module Wizard.Pages.Projects.CreateMigration.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Common.Components.TypeHintInput as TypeHintInput
import Form
import Wizard.Api.Models.KnowledgeModel exposing (KnowledgeModel)
import Wizard.Api.Models.PackageDetail exposing (PackageDetail)
import Wizard.Api.Models.PackageSuggestion exposing (PackageSuggestion)
import Wizard.Api.Models.QuestionnaireDetailWrapper exposing (QuestionnaireDetailWrapper)
import Wizard.Api.Models.QuestionnaireMigration exposing (QuestionnaireMigration)
import Wizard.Api.Models.QuestionnaireSettings exposing (QuestionnaireSettings)


type Msg
    = GetQuestionnaireCompleted (Result ApiError (QuestionnaireDetailWrapper QuestionnaireSettings))
    | Cancel
    | FormMsg Form.Msg
    | SelectPackage PackageSuggestion
    | PostMigrationCompleted (Result ApiError QuestionnaireMigration)
    | GetKnowledgeModelPreviewCompleted (Result ApiError KnowledgeModel)
    | GetCurrentPackageCompleted (Result ApiError PackageDetail)
    | GetSelectedPackageCompleted (Result ApiError PackageDetail)
    | AddTag String
    | RemoveTag String
    | ChangeUseAllQuestions Bool
    | PackageTypeHintInputMsg (TypeHintInput.Msg PackageSuggestion)
