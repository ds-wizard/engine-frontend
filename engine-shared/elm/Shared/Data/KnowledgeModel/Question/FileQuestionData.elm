module Shared.Data.KnowledgeModel.Question.FileQuestionData exposing
    ( FileQuestionData
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias FileQuestionData =
    { maxSize : Maybe Int
    , fileTypes : Maybe String
    }


decoder : Decoder FileQuestionData
decoder =
    D.succeed FileQuestionData
        |> D.required "maxSize" (D.maybe D.int)
        |> D.required "fileTypes" (D.maybe D.string)
