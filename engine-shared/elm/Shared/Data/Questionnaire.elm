module Shared.Data.Questionnaire exposing
    ( Questionnaire
    , decoder
    )

import Json.Decode as D exposing (..)
import Json.Decode.Extra as D
import Json.Decode.Pipeline exposing (optional, required)
import Shared.Data.Questionnaire.QuestionnaireAccessibility as QuestionnaireAccessibility exposing (QuestionnaireAccessibility)
import Shared.Data.Questionnaire.QuestionnaireState as QuestionnaireState exposing (QuestionnaireState)
import Time


type alias Questionnaire =
    { uuid : String
    , name : String
    , level : Int
    , accessibility : QuestionnaireAccessibility
    , state : QuestionnaireState
    , updatedAt : Time.Posix
    }


decoder : Decoder Questionnaire
decoder =
    D.succeed Questionnaire
        |> required "uuid" D.string
        |> required "name" D.string
        |> optional "level" D.int 0
        |> required "accessibility" QuestionnaireAccessibility.decoder
        |> required "state" QuestionnaireState.decoder
        |> required "updatedAt" D.datetime
