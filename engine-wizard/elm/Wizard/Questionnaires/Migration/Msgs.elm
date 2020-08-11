module Wizard.Questionnaires.Migration.Msgs exposing (Msg(..))

import Shared.Data.KnowledgeModel.Level exposing (Level)
import Shared.Data.QuestionnaireMigration exposing (QuestionnaireMigration)
import Shared.Error.ApiError exposing (ApiError)
import Wizard.Common.Components.Questionnaire as Questionnaire
import Wizard.Questionnaires.Common.QuestionChange exposing (QuestionChange)


type Msg
    = GetQuestionnaireMigrationCompleted (Result ApiError QuestionnaireMigration)
    | GetLevelsCompleted (Result ApiError (List Level))
    | QuestionnaireMsg Questionnaire.Msg
    | SelectChange QuestionChange
    | ResolveCurrentChange
    | UndoResolveCurrentChange
    | PutQuestionnaireContentCompleted (Result ApiError ())
    | PutQuestionnaireMigrationCompleted (Result ApiError ())
    | FinalizeMigration
    | FinalizeMigrationCompleted (Result ApiError ())
