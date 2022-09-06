module Shared.Data.SummaryReport.AnsweredIndicationData exposing
    ( AnsweredIndicationData
    , decoder
    , empty
    , encode
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as E


type alias AnsweredIndicationData =
    { answeredQuestions : Int
    , unansweredQuestions : Int
    }


empty : AnsweredIndicationData
empty =
    { answeredQuestions = 0
    , unansweredQuestions = 0
    }


decoder : Decoder AnsweredIndicationData
decoder =
    Decode.succeed AnsweredIndicationData
        |> required "answeredQuestions" Decode.int
        |> required "unansweredQuestions" Decode.int


encode : AnsweredIndicationData -> E.Value
encode data =
    E.object
        [ ( "answeredQuestions", E.int data.answeredQuestions )
        , ( "unansweredQuestions", E.int data.unansweredQuestions )
        ]
