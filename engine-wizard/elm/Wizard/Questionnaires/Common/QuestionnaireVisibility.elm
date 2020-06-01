module Wizard.Questionnaires.Common.QuestionnaireVisibility exposing
    ( QuestionnaireVisibility(..)
    , decoder
    , encode
    , field
    , formOptions
    , richFormOptions
    , toString
    , validation
    )

import Form.Error as Error exposing (ErrorValue(..))
import Form.Field as Field exposing (Field)
import Form.Validate as Validate exposing (Validation)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Shared.Locale exposing (lg)
import Shared.Provisioning exposing (Provisioning)


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


field : QuestionnaireVisibility -> Field
field =
    toString >> Field.string


validation : Validation e QuestionnaireVisibility
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


richFormOptions : { a | provisioning : Provisioning } -> List ( String, String, String )
richFormOptions appState =
    [ ( toString PrivateQuestionnaire
      , lg "questionnaireVisibility.private" appState
      , lg "questionnaireVisibility.private.description" appState
      )
    , ( toString PublicReadOnlyQuestionnaire
      , lg "questionnaireVisibility.publicReadOnly" appState
      , lg "questionnaireVisibility.publicReadOnly.description" appState
      )
    , ( toString PublicQuestionnaire
      , lg "questionnaireVisibility.public" appState
      , lg "questionnaireVisibility.public.description" appState
      )
    ]


formOptions : { a | provisioning : Provisioning } -> List ( String, String )
formOptions appState =
    [ ( toString PrivateQuestionnaire
      , lg "questionnaireVisibility.private" appState
      )
    , ( toString PublicReadOnlyQuestionnaire
      , lg "questionnaireVisibility.publicReadOnly" appState
      )
    , ( toString PublicQuestionnaire
      , lg "questionnaireVisibility.public" appState
      )
    ]
