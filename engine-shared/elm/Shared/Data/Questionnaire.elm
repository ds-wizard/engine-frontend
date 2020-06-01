module Shared.Data.Questionnaire exposing
    ( Questionnaire
    , decoder
    )

import Json.Decode as D exposing (..)
import Json.Decode.Extra as D
import Json.Decode.Pipeline exposing (optional, required)
import Shared.Data.Questionnaire.QuestionnaireState as QuestionnaireState exposing (QuestionnaireState)
import Shared.Data.Questionnaire.QuestionnaireVisibility as QuestionnaireVisibility exposing (QuestionnaireVisibility)
import Time


type alias Questionnaire =
    { uuid : String
    , name : String
    , level : Int
    , visibility : QuestionnaireVisibility
    , state : QuestionnaireState
    , updatedAt : Time.Posix
    }


decoder : Decoder Questionnaire
decoder =
    D.succeed Questionnaire
        |> required "uuid" D.string
        |> required "name" D.string
        |> optional "level" D.int 0
        |> required "visibility" QuestionnaireVisibility.decoder
        |> required "state" QuestionnaireState.decoder
        |> required "updatedAt" D.datetime
