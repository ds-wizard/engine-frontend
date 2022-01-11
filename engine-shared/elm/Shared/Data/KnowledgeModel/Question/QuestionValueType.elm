module Shared.Data.KnowledgeModel.Question.QuestionValueType exposing
    ( QuestionValueType(..)
    , decoder
    , default
    , encode
    , forceFromString
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
encode =
    E.string << toString


default : QuestionValueType
default =
    StringQuestionValueType


toString : QuestionValueType -> String
toString questionValueType =
    case questionValueType of
        StringQuestionValueType ->
            "StringQuestionValueType"

        DateQuestionValueType ->
            "DateQuestionValueType"

        NumberQuestionValueType ->
            "NumberQuestionValueType"

        TextQuestionValueType ->
            "TextQuestionValueType"


forceFromString : String -> QuestionValueType
forceFromString valueString =
    case valueString of
        "DateQuestionValueType" ->
            DateQuestionValueType

        "NumberQuestionValueType" ->
            NumberQuestionValueType

        "TextQuestionValueType" ->
            TextQuestionValueType

        _ ->
            StringQuestionValueType
