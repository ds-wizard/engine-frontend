module Wizard.Projects.Create.TemplateCreate.Msgs exposing (Msg(..))

import Form
import Shared.Data.Questionnaire exposing (Questionnaire)
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Error.ApiError exposing (ApiError)
import Wizard.Common.Components.TypeHintInput as TypeHintInput


type Msg
    = FormMsg Form.Msg
    | PostQuestionnaireCompleted (Result ApiError Questionnaire)
    | QuestionnaireTypeHintInputMsg (TypeHintInput.Msg Questionnaire)
    | GetTemplateQuestionnaireComplete (Result ApiError QuestionnaireDetail)
