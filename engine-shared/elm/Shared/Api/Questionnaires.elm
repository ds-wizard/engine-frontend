module Shared.Api.Questionnaires exposing
    ( getQuestionnaire
    , postQuestionnaire
    )

import Json.Encode as E
import Shared.Api exposing (AppStateLike, ToMsg, jwtFetch, jwtGet)
import Shared.Data.Questionnaire as Questionnaire exposing (Questionnaire)


getQuestionnaire : String -> AppStateLike a -> ToMsg Questionnaire msg -> Cmd msg
getQuestionnaire uuid =
    jwtGet ("/questionnaires/" ++ uuid) Questionnaire.decoder


postQuestionnaire : E.Value -> AppStateLike a -> ToMsg Questionnaire msg -> Cmd msg
postQuestionnaire =
    jwtFetch "/questionnaires" Questionnaire.decoder
