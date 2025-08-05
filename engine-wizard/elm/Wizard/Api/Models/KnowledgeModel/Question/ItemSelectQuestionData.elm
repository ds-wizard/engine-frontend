module Wizard.Api.Models.KnowledgeModel.Question.ItemSelectQuestionData exposing
    ( ItemSelectQuestionData
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias ItemSelectQuestionData =
    { listQuestionUuid : Maybe String
    }


decoder : Decoder ItemSelectQuestionData
decoder =
    D.succeed ItemSelectQuestionData
        |> D.required "listQuestionUuid" (D.maybe D.string)
