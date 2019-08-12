module KMEditor.Common.KnowledgeModel.Question.QuestionValueType exposing
    ( QuestionValueType(..)
    , decoder
    , encode
    , toString
    )

import Json.Decode as D exposing (Decoder)
import Json.Encode as E


type QuestionValueType
    = StringQuestionValueType
    | DateQuestionValueType
    | NumberQuestionValueType
    | TextQuestionValueType


decoder : Decoder QuestionValueType
decoder =
    D.string
        |> D.andThen
            (\str ->
                case str of
                    "StringQuestionValueType" ->
                        D.succeed StringQuestionValueType

                    "DateQuestionValueType" ->
                        D.succeed DateQuestionValueType

                    "NumberQuestionValueType" ->
                        D.succeed NumberQuestionValueType

                    "TextQuestionValueType" ->
                        D.succeed TextQuestionValueType

                    valueType ->
                        D.fail <| "Unknown value type: " ++ valueType
            )


encode : QuestionValueType -> E.Value
encode valueType =
    E.string <|
        case valueType of
            StringQuestionValueType ->
                "StringQuestionValueType"

            DateQuestionValueType ->
                "DateQuestionValueType"

            NumberQuestionValueType ->
                "NumberQuestionValueType"

            TextQuestionValueType ->
                "TextQuestionValueType"


toString : QuestionValueType -> String
toString questionValueType =
    case questionValueType of
        StringQuestionValueType ->
            "String"

        DateQuestionValueType ->
            "Date"

        NumberQuestionValueType ->
            "Number"

        TextQuestionValueType ->
            "Text"
