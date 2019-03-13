module Questionnaires.Requests exposing
    ( deleteQuestionnaire
    , getQuestionnaire
    , getQuestionnaires
    , postQuestionnaire
    , putQuestionnaire
    )

import Auth.Models exposing (Session)
import Common.Questionnaire.Models exposing (QuestionnaireDetail, questionnaireDetailDecoder)
import Http
import Json.Encode as Encode exposing (Value)
import KMEditor.Common.Models.Entities exposing (KnowledgeModel, knowledgeModelDecoder)
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


postQuestionnaire : Session -> Value -> Http.Request Questionnaire
postQuestionnaire session questionnaire =
    Requests.postWithResponse questionnaire session "/questionnaires" questionnaireDecoder


putQuestionnaire : String -> Session -> Value -> Http.Request String
putQuestionnaire uuid session questionnaire =
    Requests.put questionnaire session ("/questionnaires/" ++ uuid)
