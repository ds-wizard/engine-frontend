module Wizard.Questionnaires.Common.QuestionnaireAccessibility exposing
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
import Shared.Locale exposing (lg)
import Wizard.Common.AppState exposing (AppState)


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


formOptions : AppState -> List ( String, String, String )
formOptions appState =
    [ ( toString PrivateQuestionnaire
      , lg "questionnaireAccessibility.private" appState
      , lg "questionnaireAccessibility.private.description" appState
      )
    , ( toString PublicReadOnlyQuestionnaire
      , lg "questionnaireAccessibility.publicReadOnly" appState
      , lg "questionnaireAccessibility.publicReadOnly.description" appState
      )
    , ( toString PublicQuestionnaire
      , lg "questionnaireAccessibility.public" appState
      , lg "questionnaireAccessibility.public.description" appState
      )
    ]
