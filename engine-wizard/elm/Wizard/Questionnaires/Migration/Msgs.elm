module Wizard.Questionnaires.Migration.Msgs exposing (Msg(..))

import Shared.Error.ApiError exposing (ApiError)
import Wizard.Common.Questionnaire.Msgs
import Wizard.KMEditor.Common.KnowledgeModel.Level exposing (Level)
import Wizard.Questionnaires.Common.QuestionChange exposing (QuestionChange)
import Wizard.Questionnaires.Common.QuestionnaireMigration exposing (QuestionnaireMigration)


type Msg
    = GetQuestionnaireMigrationCompleted (Result ApiError QuestionnaireMigration)
    | GetLevelsCompleted (Result ApiError (List Level))
    | QuestionnaireMsg Wizard.Common.Questionnaire.Msgs.Msg
    | SelectChange QuestionChange
    | ResolveCurrentChange
    | UndoResolveCurrentChange
    | PutQuestionnaireCompleted (Result ApiError ())
    | PutQuestionnaireMigrationCompleted (Result ApiError ())
    | FinalizeMigration
    | FinalizeMigrationCompleted (Result ApiError ())
