module DSPlanner.Requests exposing (..)

import Auth.Models exposing (Session)
import DSPlanner.Common.Models exposing (..)
import Http
import Json.Encode exposing (Value)
import Requests


getQuestionnaires : Session -> Http.Request (List Questionnaire)
getQuestionnaires session =
    Requests.get session "/questionnaires" questionnaireListDecoder


getQuestionnaire : String -> Session -> Http.Request QuestionnaireDetail
getQuestionnaire uuid session =
    Requests.get session ("/questionnaires/" ++ uuid) questionnaireDetailDecoder


deleteQuestionnaire : String -> Session -> Http.Request String
deleteQuestionnaire uuid session =
    Requests.delete session ("/questionnaires/" ++ uuid)


postQuestionnaire : Session -> Value -> Http.Request String
postQuestionnaire session questionnaire =
    Requests.post questionnaire session "/questionnaires"


putReplies : String -> Session -> Value -> Http.Request String
putReplies uuid session replies =
    Requests.put replies session ("/questionnaires/" ++ uuid ++ "/replies")
