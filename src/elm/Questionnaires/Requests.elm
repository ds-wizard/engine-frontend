module Questionnaires.Requests exposing (..)

import Auth.Models exposing (Session)
import Http
import Json.Encode exposing (Value)
import Questionnaires.Common.Models exposing (..)
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


putValues : String -> Session -> Value -> Http.Request String
putValues uuid session values =
    Requests.put values session ("/questionnaires/" ++ uuid ++ "/values")
