module Wizard.Projects.Common.QuestionnaireDescriptor exposing
    ( QuestionnaireDescriptor
    , fromQuestionnaire
    , fromQuestionnaireDetail
    )

import Shared.Data.Questionnaire exposing (Questionnaire)
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Uuid exposing (Uuid)


type alias QuestionnaireDescriptor =
    { name : String, uuid : Uuid }


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
