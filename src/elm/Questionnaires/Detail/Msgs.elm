module Questionnaires.Detail.Msgs exposing (..)

import FormEngine.Msgs
import Jwt
import KnowledgeModels.Editor.Models.Entities exposing (Chapter)
import Questionnaires.Common.Models exposing (QuestionnaireDetail)


type Msg
    = GetQuestionnaireCompleted (Result Jwt.JwtError QuestionnaireDetail)
    | SetActiveChapter Chapter
    | FormMsg FormEngine.Msgs.Msg
