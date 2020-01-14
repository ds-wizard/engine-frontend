module Wizard.Questionnaires.CreateMigration.Msgs exposing (Msg(..))

import Form
import Shared.Error.ApiError exposing (ApiError)
import Wizard.KMEditor.Common.KnowledgeModel.KnowledgeModel exposing (KnowledgeModel)
import Wizard.KnowledgeModels.Common.Package exposing (Package)
import Wizard.Questionnaires.Common.QuestionnaireDetail exposing (QuestionnaireDetail)
import Wizard.Questionnaires.Common.QuestionnaireMigration exposing (QuestionnaireMigration)


type Msg
    = GetPackagesCompleted (Result ApiError (List Package))
    | GetQuestionnaireCompleted (Result ApiError QuestionnaireDetail)
    | FormMsg Form.Msg
    | SelectPackage String
    | PostMigrationCompleted (Result ApiError QuestionnaireMigration)
    | GetKnowledgeModelPreviewCompleted (Result ApiError KnowledgeModel)
    | AddTag String
    | RemoveTag String
