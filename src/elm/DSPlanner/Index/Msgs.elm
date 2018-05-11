module DSPlanner.Index.Msgs exposing (..)

import DSPlanner.Common.Models exposing (Questionnaire)
import Jwt


type Msg
    = GetQuestionnairesCompleted (Result Jwt.JwtError (List Questionnaire))
    | ShowHideDeleteQuestionnaire (Maybe Questionnaire)
    | DeleteQuestionnaire
    | DeleteQuestionnaireCompleted (Result Jwt.JwtError String)
