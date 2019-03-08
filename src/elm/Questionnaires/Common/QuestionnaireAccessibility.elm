module Questionnaires.Common.QuestionnaireAccessibility exposing
    ( QuestionnaireAccessibility(..)
    , decoder
    , encode
    , formOptions
    , toString
    , validation
    )

import Form.Error as Error exposing (ErrorValue(..))
import Form.Validate as Validate exposing (Validation)
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
    [ ( toString PrivateQuestionnaire, "Private", "Questionnaire is visible only to you." )
    , ( toString PublicReadOnlyQuestionnaire, "Public Read-Only", "Questionnaire can be viewed by other users, but they cannot change it." )
    , ( toString PublicQuestionnaire, "Public", "Questionnaire can be accessed by all users." )
    ]
