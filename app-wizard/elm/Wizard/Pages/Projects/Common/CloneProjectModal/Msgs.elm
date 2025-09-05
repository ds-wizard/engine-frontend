module Wizard.Pages.Projects.Common.CloneProjectModal.Msgs exposing (Msg(..))

import Common.Data.ApiError exposing (ApiError)
import Wizard.Api.Models.Questionnaire exposing (Questionnaire)
import Wizard.Pages.Projects.Common.QuestionnaireDescriptor exposing (QuestionnaireDescriptor)


type Msg
    = ShowHideCloneQuestionnaire (Maybe QuestionnaireDescriptor)
    | CloneQuestionnaire
    | CloneQuestionnaireCompleted (Result ApiError Questionnaire)
