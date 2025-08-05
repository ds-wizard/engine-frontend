module Wizard.Api.Models.SummaryReport.AnsweredIndicationData exposing
    ( AnsweredIndicationData
    , decoder
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)


type alias AnsweredIndicationData =
    { answeredQuestions : Int
    , unansweredQuestions : Int
    }


decoder : Decoder AnsweredIndicationData
decoder =
    Decode.succeed AnsweredIndicationData
        |> required "answeredQuestions" Decode.int
        |> required "unansweredQuestions" Decode.int
