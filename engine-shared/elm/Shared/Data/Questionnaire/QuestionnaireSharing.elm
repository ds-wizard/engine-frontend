module Shared.Data.Questionnaire.QuestionnaireSharing exposing
    ( QuestionnaireSharing(..)
    , decoder
    , encode
    , field
    , fromFormValues
    , richFormOptions
    , toFormValues
    , validation
    )

import Form.Error as Error exposing (ErrorValue(..))
import Form.Field as Field exposing (Field)
import Form.Validate as Validate exposing (Validation)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Shared.Data.QuestionnairePermission as QuestionnairePermission exposing (QuestionnairePermission)
import Shared.Locale exposing (lg)
import Shared.Provisioning exposing (Provisioning)


type QuestionnaireSharing
    = RestrictedQuestionnaire
    | AnyoneWithLinkViewQuestionnaire
    | AnyoneWithLinkCommentQuestionnaire
    | AnyoneWithLinkEditQuestionnaire


toString : QuestionnaireSharing -> String
toString questionnaireSharing =
    case questionnaireSharing of
        RestrictedQuestionnaire ->
            "RestrictedQuestionnaire"

        AnyoneWithLinkViewQuestionnaire ->
            "AnyoneWithLinkViewQuestionnaire"

        AnyoneWithLinkCommentQuestionnaire ->
            "AnyoneWithLinkCommentQuestionnaire"

        AnyoneWithLinkEditQuestionnaire ->
            "AnyoneWithLinkEditQuestionnaire"


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

                    "AnyoneWithLinkViewQuestionnaire" ->
                        D.succeed AnyoneWithLinkViewQuestionnaire

                    "AnyoneWithLinkCommentQuestionnaire" ->
                        D.succeed AnyoneWithLinkCommentQuestionnaire

                    "AnyoneWithLinkEditQuestionnaire" ->
                        D.succeed AnyoneWithLinkEditQuestionnaire

                    valueType ->
                        D.fail <| "Unknown questionnaire sharing: " ++ valueType
            )


toFormValues : QuestionnaireSharing -> ( Bool, QuestionnairePermission )
toFormValues sharing =
    case sharing of
        RestrictedQuestionnaire ->
            ( False, QuestionnairePermission.View )

        AnyoneWithLinkViewQuestionnaire ->
            ( True, QuestionnairePermission.View )

        AnyoneWithLinkCommentQuestionnaire ->
            ( True, QuestionnairePermission.Comment )

        AnyoneWithLinkEditQuestionnaire ->
            ( True, QuestionnairePermission.Edit )


fromFormValues : Bool -> QuestionnairePermission -> QuestionnaireSharing
fromFormValues enabled perm =
    if enabled then
        if perm == QuestionnairePermission.Edit then
            AnyoneWithLinkEditQuestionnaire

        else if perm == QuestionnairePermission.Comment then
            AnyoneWithLinkCommentQuestionnaire

        else
            AnyoneWithLinkViewQuestionnaire

    else
        RestrictedQuestionnaire


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

                    "AnyoneWithLinkViewQuestionnaire" ->
                        Validate.succeed AnyoneWithLinkViewQuestionnaire

                    "AnyoneWithLinkCommentQuestionnaire" ->
                        Validate.succeed AnyoneWithLinkCommentQuestionnaire

                    "AnyoneWithLinkEditQuestionnaire" ->
                        Validate.succeed AnyoneWithLinkEditQuestionnaire

                    _ ->
                        Validate.fail <| Error.value InvalidString
            )


richFormOptions : { a | provisioning : Provisioning } -> List ( String, String, String )
richFormOptions appState =
    [ ( toString RestrictedQuestionnaire
      , lg "questionnaireSharing.restricted" appState
      , lg "questionnaireSharing.restricted.description" appState
      )
    , ( toString AnyoneWithLinkViewQuestionnaire
      , lg "questionnaireSharing.anyoneWithLinkView" appState
      , lg "questionnaireSharing.anyoneWithLinkView.description" appState
      )
    , ( toString AnyoneWithLinkCommentQuestionnaire
      , lg "questionnaireSharing.anyoneWithLinkComment" appState
      , lg "questionnaireSharing.anyoneWithLinkComment.description" appState
      )
    , ( toString AnyoneWithLinkEditQuestionnaire
      , lg "questionnaireSharing.anyoneWithLinkEdit" appState
      , lg "questionnaireSharing.anyoneWithLinkEdit.description" appState
      )
    ]
