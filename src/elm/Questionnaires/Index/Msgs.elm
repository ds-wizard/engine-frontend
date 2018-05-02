module Questionnaires.Index.Msgs exposing (..)

import Jwt
import Questionnaires.Common.Models exposing (Questionnaire)


type Msg
    = GetQuestionnairesCompleted (Result Jwt.JwtError (List Questionnaire))
    | ShowHideDeleteQuestionnaire (Maybe Questionnaire)
