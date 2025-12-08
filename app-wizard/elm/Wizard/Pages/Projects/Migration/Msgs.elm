module Wizard.Pages.Projects.Migration.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Wizard.Api.Models.ProjectMigration exposing (ProjectMigration)
import Wizard.Components.Questionnaire as Questionnaire
import Wizard.Pages.Projects.Common.QuestionChange exposing (QuestionChange)


type Msg
    = GetQuestionnaireMigrationCompleted (Result ApiError ProjectMigration)
    | QuestionnaireMsg Questionnaire.Msg
    | SelectChange QuestionChange
    | ResolveCurrentChange
    | ResolveAllChanges
    | UndoResolveCurrentChange
    | PutQuestionnaireContentCompleted
    | PutQuestionnaireMigrationCompleted (Result ApiError ())
    | FinalizeMigration
    | FinalizeMigrationCompleted (Result ApiError ())
