module Wizard.Projects.Migration.Msgs exposing (Msg(..))

import Shared.Data.QuestionnaireMigration exposing (QuestionnaireMigration)
import Shared.Error.ApiError exposing (ApiError)
import Wizard.Common.Components.Questionnaire as Questionnaire
import Wizard.Projects.Common.QuestionChange exposing (QuestionChange)


type Msg
    = GetQuestionnaireMigrationCompleted (Result ApiError QuestionnaireMigration)
    | QuestionnaireMsg Questionnaire.Msg
    | SelectChange QuestionChange
    | ResolveCurrentChange
    | UndoResolveCurrentChange
    | PutQuestionnaireContentCompleted (Result ApiError ())
    | PutQuestionnaireMigrationCompleted (Result ApiError ())
    | FinalizeMigration
    | FinalizeMigrationCompleted (Result ApiError ())
