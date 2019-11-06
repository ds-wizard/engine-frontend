module Wizard.KMEditor.Common.KnowledgeModel.Question.ValueQuestionData exposing
    ( ValueQuestionData
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Wizard.KMEditor.Common.KnowledgeModel.Question.QuestionValueType as QuestionValueType exposing (QuestionValueType)


type alias ValueQuestionData =
    { valueType : QuestionValueType
    }


decoder : Decoder ValueQuestionData
decoder =
    D.succeed ValueQuestionData
        |> D.required "valueType" QuestionValueType.decoder
