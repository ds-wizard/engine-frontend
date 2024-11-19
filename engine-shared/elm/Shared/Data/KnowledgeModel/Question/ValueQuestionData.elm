module Shared.Data.KnowledgeModel.Question.ValueQuestionData exposing
    ( ValueQuestionData
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Data.KnowledgeModel.Question.QuestionValidation as QuestionValidation exposing (QuestionValidation)
import Shared.Data.KnowledgeModel.Question.QuestionValueType as QuestionValueType exposing (QuestionValueType)


type alias ValueQuestionData =
    { valueType : QuestionValueType
    , validations : List QuestionValidation
    }


decoder : Decoder ValueQuestionData
decoder =
    D.succeed ValueQuestionData
        |> D.required "valueType" QuestionValueType.decoder
        |> D.required "validations" (D.list QuestionValidation.decoder)
