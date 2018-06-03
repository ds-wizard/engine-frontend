module DSPlanner.Index.Msgs exposing (..)

import Bootstrap.Dropdown as Dropdown
import DSPlanner.Common.Models exposing (Questionnaire)
import Jwt


type Msg
    = GetQuestionnairesCompleted (Result Jwt.JwtError (List Questionnaire))
    | ShowHideDeleteQuestionnaire (Maybe Questionnaire)
    | DeleteQuestionnaire
    | DeleteQuestionnaireCompleted (Result Jwt.JwtError String)
    | DropdownMsg Questionnaire Dropdown.State
