module Questionnaires.Migration.Msgs exposing (Msg(..))

import Common.ApiError exposing (ApiError)
import Common.Questionnaire.Msgs
import KMEditor.Common.Models.Entities exposing (Level)
import Questionnaires.Common.QuestionChange exposing (QuestionChange)
import Questionnaires.Common.QuestionnaireMigration exposing (QuestionnaireMigration)


type Msg
    = GetQuestionnaireMigrationCompleted (Result ApiError QuestionnaireMigration)
    | GetLevelsCompleted (Result ApiError (List Level))
    | QuestionnaireMsg Common.Questionnaire.Msgs.Msg
    | SelectChange QuestionChange
    | ResolveCurrentChange
    | UndoResolveCurrentChange
    | PutQuestionnaireCompleted (Result ApiError ())
    | PutQuestionnaireMigrationCompleted (Result ApiError ())
    | FinalizeMigration
    | FinalizeMigrationCompleted (Result ApiError ())
