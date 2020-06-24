module Wizard.Questionnaires.CreateMigration.Msgs exposing (Msg(..))

import Form
import Shared.Data.KnowledgeModel exposing (KnowledgeModel)
import Shared.Data.Package exposing (Package)
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Data.QuestionnaireMigration exposing (QuestionnaireMigration)
import Shared.Error.ApiError exposing (ApiError)


type Msg
    = GetPackagesCompleted (Result ApiError (List Package))
    | GetQuestionnaireCompleted (Result ApiError QuestionnaireDetail)
    | FormMsg Form.Msg
    | SelectPackage String
    | PostMigrationCompleted (Result ApiError QuestionnaireMigration)
    | GetKnowledgeModelPreviewCompleted (Result ApiError KnowledgeModel)
    | AddTag String
    | RemoveTag String
