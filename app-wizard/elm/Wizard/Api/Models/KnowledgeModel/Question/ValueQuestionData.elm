module Wizard.Api.Models.KnowledgeModel.Question.ValueQuestionData exposing
    ( ValueQuestionData
    , decoder
    , encodeValues
    , equalContent
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
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


encodeValues : ValueQuestionData -> List ( String, E.Value )
encodeValues valueData =
    [ ( "valueType", QuestionValueType.encode valueData.valueType ) ]


equalContent : ValueQuestionData -> ValueQuestionData -> Bool
equalContent data1 data2 =
    data1.valueType == data2.valueType
