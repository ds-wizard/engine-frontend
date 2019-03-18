module Questionnaires.Index.Msgs exposing (Msg(..))

import Jwt
import Questionnaires.Common.Models exposing (Questionnaire)


type Msg
    = GetQuestionnairesCompleted (Result Jwt.JwtError (List Questionnaire))
    | ShowHideDeleteQuestionnaire (Maybe Questionnaire)
    | DeleteQuestionnaire
    | DeleteQuestionnaireCompleted (Result Jwt.JwtError String)
    | ShowHideExportQuestionnaire (Maybe Questionnaire)
