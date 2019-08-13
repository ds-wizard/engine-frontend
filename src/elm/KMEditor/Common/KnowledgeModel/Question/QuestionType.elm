module KMEditor.Common.KnowledgeModel.Question.QuestionType exposing
    ( QuestionType(..)
    , decoder
    , toString
    )

import Json.Decode as D exposing (Decoder)


type QuestionType
    = OptionsQuestionType
    | ListQuestionType
    | ValueQuestionType
    | IntegrationQuestionType


decoder : Decoder QuestionType
decoder =
    D.field "questionType" D.string
        |> D.andThen
            (\str ->
                case str of
                    "OptionsQuestion" ->
                        D.succeed OptionsQuestionType

                    "ListQuestion" ->
                        D.succeed ListQuestionType

                    "ValueQuestion" ->
                        D.succeed ValueQuestionType

                    "IntegrationQuestion" ->
                        D.succeed IntegrationQuestionType

                    valueType ->
                        D.fail <| "Unknown question type: " ++ valueType
            )


toString : QuestionType -> String
toString questionType =
    case questionType of
        OptionsQuestionType ->
            "Options"

        ListQuestionType ->
            "List"

        ValueQuestionType ->
            "Value"

        IntegrationQuestionType ->
            "Integration"
