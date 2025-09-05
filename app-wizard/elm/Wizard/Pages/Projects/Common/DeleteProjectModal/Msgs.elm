module Wizard.Pages.Projects.Common.DeleteProjectModal.Msgs exposing (Msg(..))

import Common.Data.ApiError exposing (ApiError)
import Wizard.Pages.Projects.Common.QuestionnaireDescriptor exposing (QuestionnaireDescriptor)


type Msg
    = ShowHideDeleteQuestionnaire (Maybe QuestionnaireDescriptor)
    | DeleteQuestionnaire
    | DeleteQuestionnaireCompleted (Result ApiError ())
