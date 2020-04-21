module Shared.Data.Questionnaire.QuestionnaireAccessibility exposing
    ( QuestionnaireAccessibility(..)
    , decoder
    , encode
    , toString
    )

import Json.Decode as D exposing (Decoder)
import Json.Encode as E


type QuestionnaireAccessibility
    = PublicQuestionnaire
    | PrivateQuestionnaire
    | PublicReadOnlyQuestionnaire


toString : QuestionnaireAccessibility -> String
toString questionnaireAccessibility =
    case questionnaireAccessibility of
        PublicQuestionnaire ->
            "PublicQuestionnaire"

        PublicReadOnlyQuestionnaire ->
            "PublicReadOnlyQuestionnaire"

        PrivateQuestionnaire ->
            "PrivateQuestionnaire"


encode : QuestionnaireAccessibility -> E.Value
encode =
    E.string << toString


decoder : Decoder QuestionnaireAccessibility
decoder =
    D.string
        |> D.andThen
            (\str ->
                case str of
                    "PublicQuestionnaire" ->
                        D.succeed PublicQuestionnaire

                    "PrivateQuestionnaire" ->
                        D.succeed PrivateQuestionnaire

                    "PublicReadOnlyQuestionnaire" ->
                        D.succeed PublicReadOnlyQuestionnaire

                    valueType ->
                        D.fail <| "Unknown questionnaire accessibility: " ++ valueType
            )
