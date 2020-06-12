module Wizard.Questionnaires.Common.DeleteQuestionnaireModal.QuestionnaireDescriptor exposing
    ( QuestionnaireDescriptor
    , fromQuestionnaire
    , fromQuestionnaireDetail
    )

import Wizard.Questionnaires.Common.Questionnaire exposing (Questionnaire)
import Wizard.Questionnaires.Common.QuestionnaireDetail exposing (QuestionnaireDetail)


type alias QuestionnaireDescriptor =
    { name : String, uuid : String }


fromQuestionnaire : Questionnaire -> QuestionnaireDescriptor
fromQuestionnaire questionnaire =
    { name = questionnaire.name
    , uuid = questionnaire.uuid
    }


fromQuestionnaireDetail : QuestionnaireDetail -> QuestionnaireDescriptor
fromQuestionnaireDetail questionnaire =
    { name = questionnaire.name
    , uuid = questionnaire.uuid
    }
