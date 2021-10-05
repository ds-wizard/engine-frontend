module Wizard.Projects.Common.CloneProjectModal.Msgs exposing (Msg(..))

import Shared.Data.Questionnaire exposing (Questionnaire)
import Shared.Error.ApiError exposing (ApiError)
import Wizard.Projects.Common.QuestionnaireDescriptor exposing (QuestionnaireDescriptor)


type Msg
    = ShowHideCloneQuestionnaire (Maybe QuestionnaireDescriptor)
    | CloneQuestionnaire
    | CloneQuestionnaireCompleted (Result ApiError Questionnaire)
