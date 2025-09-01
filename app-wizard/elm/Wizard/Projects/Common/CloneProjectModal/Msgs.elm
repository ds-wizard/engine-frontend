module Wizard.Projects.Common.CloneProjectModal.Msgs exposing (Msg(..))

import Shared.Data.ApiError exposing (ApiError)
import Wizard.Api.Models.Questionnaire exposing (Questionnaire)
import Wizard.Projects.Common.QuestionnaireDescriptor exposing (QuestionnaireDescriptor)


type Msg
    = ShowHideCloneQuestionnaire (Maybe QuestionnaireDescriptor)
    | CloneQuestionnaire
    | CloneQuestionnaireCompleted (Result ApiError Questionnaire)
