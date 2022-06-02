module Shared.Data.KnowledgeModel.Question.QuestionType exposing
    ( QuestionType(..)
    , decoder
    )

import Json.Decode as D exposing (Decoder)


type QuestionType
    = OptionsQuestionType
    | ListQuestionType
    | ValueQuestionType
    | IntegrationQuestionType
    | MultiChoiceQuestionType


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

                    "MultiChoiceQuestion" ->
                        D.succeed MultiChoiceQuestionType

                    valueType ->
                        D.fail <| "Unknown question type: " ++ valueType
            )
