module Wizard.Questionnaires.Common.QuestionnairePagination exposing
    ( QuestionnairePagination
    , decoder
    , getQuestionnaires
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Wizard.Common.Pagination.Pagination as Pagination exposing (Pagination)
import Wizard.Questionnaires.Common.Questionnaire as Questionnaire exposing (Questionnaire)


type alias QuestionnairePagination =
    Pagination Embedded


type alias Embedded =
    { questionnaires : List Questionnaire }


decoder : Decoder QuestionnairePagination
decoder =
    Pagination.decoder embeddedDecoder


embeddedDecoder : Decoder Embedded
embeddedDecoder =
    D.succeed Embedded
        |> D.required "questionnaires" (D.list Questionnaire.decoder)


getQuestionnaires : QuestionnairePagination -> List Questionnaire
getQuestionnaires =
    .embedded >> .questionnaires
