module Shared.Data.Questionnaire.QuestionnaireVisibility exposing
    ( QuestionnaireVisibility(..)
    , decoder
    , encode
    , toString
    )

import Json.Decode as D exposing (Decoder)
import Json.Encode as E


type QuestionnaireVisibility
    = PublicQuestionnaire
    | PrivateQuestionnaire
    | PublicReadOnlyQuestionnaire


toString : QuestionnaireVisibility -> String
toString questionnaireVisibility =
    case questionnaireVisibility of
        PublicQuestionnaire ->
            "PublicQuestionnaire"

        PublicReadOnlyQuestionnaire ->
            "PublicReadOnlyQuestionnaire"

        PrivateQuestionnaire ->
            "PrivateQuestionnaire"


encode : QuestionnaireVisibility -> E.Value
encode =
    E.string << toString


decoder : Decoder QuestionnaireVisibility
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
                        D.fail <| "Unknown questionnaire visibility: " ++ valueType
            )
