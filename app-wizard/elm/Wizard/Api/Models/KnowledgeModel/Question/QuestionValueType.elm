module Wizard.Api.Models.KnowledgeModel.Question.QuestionValueType exposing
    ( QuestionValueType(..)
    , decoder
    , default
    , encode
    , fromString
    , toString
    )

import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Maybe.Extra as Maybe


type QuestionValueType
    = StringQuestionValueType
    | NumberQuestionValueType
    | DateQuestionValueType
    | DateTimeQuestionValueType
    | TimeQuestionValueType
    | TextQuestionValueType
    | EmailQuestionValueType
    | UrlQuestionValueType
    | ColorQuestionValueType


decoder : Decoder QuestionValueType
decoder =
    D.string
        |> D.andThen
            (\str ->
                let
                    fail =
                        D.fail <| "Unknown value type: " ++ str
                in
                Maybe.unwrap fail D.succeed (fromString str)
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

        NumberQuestionValueType ->
            "NumberQuestionValueType"

        DateQuestionValueType ->
            "DateQuestionValueType"

        DateTimeQuestionValueType ->
            "DateTimeQuestionValueType"

        TimeQuestionValueType ->
            "TimeQuestionValueType"

        TextQuestionValueType ->
            "TextQuestionValueType"

        EmailQuestionValueType ->
            "EmailQuestionValueType"

        UrlQuestionValueType ->
            "UrlQuestionValueType"

        ColorQuestionValueType ->
            "ColorQuestionValueType"


fromString : String -> Maybe QuestionValueType
fromString valueString =
    case valueString of
        "StringQuestionValueType" ->
            Just StringQuestionValueType

        "NumberQuestionValueType" ->
            Just NumberQuestionValueType

        "DateQuestionValueType" ->
            Just DateQuestionValueType

        "DateTimeQuestionValueType" ->
            Just DateTimeQuestionValueType

        "TimeQuestionValueType" ->
            Just TimeQuestionValueType

        "TextQuestionValueType" ->
            Just TextQuestionValueType

        "EmailQuestionValueType" ->
            Just EmailQuestionValueType

        "UrlQuestionValueType" ->
            Just UrlQuestionValueType

        "ColorQuestionValueType" ->
            Just ColorQuestionValueType

        _ ->
            Nothing
