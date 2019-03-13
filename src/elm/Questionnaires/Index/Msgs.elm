module Questionnaires.Index.Msgs exposing (Msg(..))

import Bootstrap.Dropdown as Dropdown
import Jwt
import Questionnaires.Common.Models exposing (Questionnaire)


type Msg
    = GetQuestionnairesCompleted (Result Jwt.JwtError (List Questionnaire))
    | ShowHideDeleteQuestionnaire (Maybe Questionnaire)
    | DeleteQuestionnaire
    | DeleteQuestionnaireCompleted (Result Jwt.JwtError String)
    | DropdownMsg Questionnaire Dropdown.State
