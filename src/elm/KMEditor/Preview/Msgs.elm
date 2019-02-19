module KMEditor.Preview.Msgs exposing (Msg(..))

import Common.Questionnaire.Msgs
import Jwt
import KMEditor.Common.Models.Entities exposing (KnowledgeModel, Level)


type Msg
    = GetKnowledgeModelCompleted (Result Jwt.JwtError KnowledgeModel)
    | GetLevelsCompleted (Result Jwt.JwtError (List Level))
    | QuestionnaireMsg Common.Questionnaire.Msgs.Msg
