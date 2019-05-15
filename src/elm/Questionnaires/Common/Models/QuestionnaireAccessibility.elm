module Questionnaires.Common.Models.QuestionnaireAccessibility exposing
    ( QuestionnaireAccessibility(..)
    , decoder
    , encode
    , formOptions
    , toString
    , validation
    )

import Form.Error as Error exposing (ErrorValue(..))
import Form.Validate as Validate exposing (Validation)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


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


encode : QuestionnaireAccessibility -> Encode.Value
encode =
    Encode.string << toString


decoder : Decoder QuestionnaireAccessibility
decoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "PublicQuestionnaire" ->
                        Decode.succeed PublicQuestionnaire

                    "PrivateQuestionnaire" ->
                        Decode.succeed PrivateQuestionnaire

                    "PublicReadOnlyQuestionnaire" ->
                        Decode.succeed PublicReadOnlyQuestionnaire

                    valueType ->
                        Decode.fail <| "Unknown questionnaire accessibility: " ++ valueType
            )


validation : Validation e QuestionnaireAccessibility
validation =
    Validate.string
        |> Validate.andThen
            (\valueType ->
                case valueType of
                    "PublicQuestionnaire" ->
                        Validate.succeed PublicQuestionnaire

                    "PrivateQuestionnaire" ->
                        Validate.succeed PrivateQuestionnaire

                    "PublicReadOnlyQuestionnaire" ->
                        Validate.succeed PublicReadOnlyQuestionnaire

                    _ ->
                        Validate.fail <| Error.value InvalidString
            )


formOptions : List ( String, String, String )
formOptions =
    [ ( toString PublicQuestionnaire, "Public", "Questionnaire can be accessed by all users." )
    , ( toString PublicReadOnlyQuestionnaire, "Public Read-Only", "Questionnaire can be viewed by other users, but they cannot change it." )
    , ( toString PrivateQuestionnaire, "Private", "Questionnaire is visible only to you." )
    ]
