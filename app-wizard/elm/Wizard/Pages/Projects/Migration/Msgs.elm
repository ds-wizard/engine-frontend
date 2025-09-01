module Wizard.Pages.Projects.Migration.Msgs exposing (Msg(..))

import Shared.Data.ApiError exposing (ApiError)
import Wizard.Api.Models.QuestionnaireMigration exposing (QuestionnaireMigration)
import Wizard.Components.Questionnaire as Questionnaire
import Wizard.Pages.Projects.Common.QuestionChange exposing (QuestionChange)


type Msg
    = GetQuestionnaireMigrationCompleted (Result ApiError QuestionnaireMigration)
    | QuestionnaireMsg Questionnaire.Msg
    | SelectChange QuestionChange
    | ResolveCurrentChange
    | ResolveAllChanges
    | UndoResolveCurrentChange
    | PutQuestionnaireContentCompleted
    | PutQuestionnaireMigrationCompleted (Result ApiError ())
    | FinalizeMigration
    | FinalizeMigrationCompleted (Result ApiError ())
