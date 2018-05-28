module DSPlanner.Detail.Msgs exposing (..)

import DSPlanner.Common.Models exposing (QuestionnaireDetail)
import FormEngine.Msgs
import Jwt
import KMEditor.Common.Models.Entities exposing (Chapter)


type Msg
    = GetQuestionnaireCompleted (Result Jwt.JwtError QuestionnaireDetail)
    | SetActiveChapter Chapter
    | FormMsg FormEngine.Msgs.Msg
    | Save
    | PutRepliesCompleted (Result Jwt.JwtError String)
