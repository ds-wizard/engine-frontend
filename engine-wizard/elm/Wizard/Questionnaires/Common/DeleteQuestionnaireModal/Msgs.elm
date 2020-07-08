module Wizard.Questionnaires.Common.DeleteQuestionnaireModal.Msgs exposing (..)

import Shared.Error.ApiError exposing (ApiError)
import Wizard.Questionnaires.Common.DeleteQuestionnaireModal.QuestionnaireDescriptor exposing (QuestionnaireDescriptor)


type Msg
    = ShowHideDeleteQuestionnaire (Maybe QuestionnaireDescriptor)
    | DeleteQuestionnaire
    | DeleteQuestionnaireCompleted (Result ApiError ())
