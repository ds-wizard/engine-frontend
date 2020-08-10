module Shared.Data.Questionnaire.QuestionnaireSharing exposing
    ( QuestionnaireSharing(..)
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
import Shared.Data.Questionnaire.QuestionnaireVisibility exposing (QuestionnaireVisibility(..))
import Shared.Locale exposing (lg, lgf)
import Shared.Provisioning exposing (Provisioning)


type QuestionnaireSharing
    = RestrictedQuestionnaire
    | AnyoneWithLinkQuestionnaire


toString : QuestionnaireSharing -> String
toString questionnaireSharing =
    case questionnaireSharing of
        RestrictedQuestionnaire ->
            "RestrictedQuestionnaire"

        AnyoneWithLinkQuestionnaire ->
            "AnyoneWithLinkQuestionnaire"


encode : QuestionnaireSharing -> E.Value
encode =
    E.string << toString


decoder : Decoder QuestionnaireSharing
decoder =
    D.string
        |> D.andThen
            (\str ->
                case str of
                    "RestrictedQuestionnaire" ->
                        D.succeed RestrictedQuestionnaire

                    "AnyoneWithLinkQuestionnaire" ->
                        D.succeed AnyoneWithLinkQuestionnaire

                    valueType ->
                        D.fail <| "Unknown questionnaire sharing: " ++ valueType
            )


field : QuestionnaireSharing -> Field
field =
    toString >> Field.string


validation : Validation e QuestionnaireSharing
validation =
    Validate.string
        |> Validate.andThen
            (\valueType ->
                case valueType of
                    "RestrictedQuestionnaire" ->
                        Validate.succeed RestrictedQuestionnaire

                    "AnyoneWithLinkQuestionnaire" ->
                        Validate.succeed AnyoneWithLinkQuestionnaire

                    _ ->
                        Validate.fail <| Error.value InvalidString
            )


richFormOptions : { a | provisioning : Provisioning } -> QuestionnaireVisibility -> List ( String, String, String )
richFormOptions appState visibility =
    [ ( toString RestrictedQuestionnaire
      , lg "questionnaireSharing.restricted" appState
      , lgf "questionnaireSharing.restricted.description" [ allowedAction appState visibility ] appState
      )
    , ( toString AnyoneWithLinkQuestionnaire
      , lg "questionnaireSharing.anyoneWithLink" appState
      , lgf "questionnaireSharing.anyoneWithLink.description" [ allowedAction appState visibility ] appState
      )
    ]


formOptions : { a | provisioning : Provisioning } -> List ( String, String )
formOptions appState =
    [ ( toString RestrictedQuestionnaire
      , lg "questionnaireSharing.restricted" appState
      )
    , ( toString AnyoneWithLinkQuestionnaire
      , lg "questionnaireSharing.anyoneWithLink" appState
      )
    ]


allowedAction : { a | provisioning : Provisioning } -> QuestionnaireVisibility -> String
allowedAction appState visibility =
    case visibility of
        PublicQuestionnaire ->
            lg "questionnaireSharing.action.edit" appState

        _ ->
            lg "questionnaireSharing.action.view" appState
