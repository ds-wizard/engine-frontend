module Wizard.Projects.Common.QuestionnaireDescriptor exposing
    ( QuestionnaireDescriptor
    , fromQuestionnaire
    , fromQuestionnaireSettings
    )

import Uuid exposing (Uuid)
import Wizard.Api.Models.Questionnaire exposing (Questionnaire)
import Wizard.Api.Models.QuestionnaireSettings exposing (QuestionnaireSettings)


type alias QuestionnaireDescriptor =
    { name : String
    , uuid : Uuid
    }


fromQuestionnaire : Questionnaire -> QuestionnaireDescriptor
fromQuestionnaire questionnaire =
    { name = questionnaire.name
    , uuid = questionnaire.uuid
    }


fromQuestionnaireSettings : QuestionnaireSettings -> QuestionnaireDescriptor
fromQuestionnaireSettings settings =
    { name = settings.name
    , uuid = settings.uuid
    }
