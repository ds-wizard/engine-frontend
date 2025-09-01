module Wizard.Api.Models.KnowledgeModel.Question.ValueQuestionData exposing
    ( ValueQuestionData
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Wizard.Api.Models.KnowledgeModel.Question.QuestionValidation as QuestionValidation exposing (QuestionValidation)
import Wizard.Api.Models.KnowledgeModel.Question.QuestionValueType as QuestionValueType exposing (QuestionValueType)


type alias ValueQuestionData =
    { valueType : QuestionValueType
    , validations : List QuestionValidation
    }


decoder : Decoder ValueQuestionData
decoder =
    D.succeed ValueQuestionData
        |> D.required "valueType" QuestionValueType.decoder
        |> D.required "validations" (D.list QuestionValidation.decoder)
