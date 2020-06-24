module Shared.Data.KnowledgeModel.Question.OptionsQuestionData exposing
    ( OptionsQuestionData
    , decoder
    , new
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias OptionsQuestionData =
    { answerUuids : List String
    }


decoder : Decoder OptionsQuestionData
decoder =
    D.succeed OptionsQuestionData
        |> D.required "answerUuids" (D.list D.string)


new : OptionsQuestionData
new =
    { answerUuids = []
    }
