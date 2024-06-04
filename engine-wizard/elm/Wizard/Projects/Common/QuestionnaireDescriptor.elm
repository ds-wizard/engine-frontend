module Wizard.Projects.Common.QuestionnaireDescriptor exposing
    ( QuestionnaireDescriptor
    , fromQuestionnaire
    , fromQuestionnaireSettings
    )

import Shared.Data.Questionnaire exposing (Questionnaire)
import Shared.Data.QuestionnaireSettings exposing (QuestionnaireSettings)
import Uuid exposing (Uuid)


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
