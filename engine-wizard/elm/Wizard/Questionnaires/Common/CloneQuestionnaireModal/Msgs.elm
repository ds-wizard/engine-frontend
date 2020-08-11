module Wizard.Questionnaires.Common.CloneQuestionnaireModal.Msgs exposing (..)

import Shared.Data.Questionnaire exposing (Questionnaire)
import Shared.Error.ApiError exposing (ApiError)
import Wizard.Questionnaires.Common.QuestionnaireDescriptor exposing (QuestionnaireDescriptor)


type Msg
    = ShowHideCloneQuestionnaire (Maybe QuestionnaireDescriptor)
    | CloneQuestionnaire
    | CloneQuestionnaireCompleted (Result ApiError Questionnaire)
