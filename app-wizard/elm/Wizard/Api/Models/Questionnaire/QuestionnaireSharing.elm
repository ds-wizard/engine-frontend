module Wizard.Api.Models.Questionnaire.QuestionnaireSharing exposing
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
import Gettext exposing (gettext)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Wizard.Api.Models.QuestionnairePermission as QuestionnairePermission exposing (QuestionnairePermission)


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


richFormOptions : { a | locale : Gettext.Locale } -> List ( String, String, String )
richFormOptions appState =
    [ ( toString RestrictedQuestionnaire
      , gettext "Restricted" appState.locale
      , gettext "Only logged-in users can access the project depending on the project visibility." appState.locale
      )
    , ( toString AnyoneWithLinkViewQuestionnaire
      , gettext "View with the link" appState.locale
      , gettext "Anyone on the internet with the link can view." appState.locale
      )
    , ( toString AnyoneWithLinkCommentQuestionnaire
      , gettext "Comment with the link" appState.locale
      , gettext "Anyone on the internet with the link can view and comment." appState.locale
      )
    , ( toString AnyoneWithLinkEditQuestionnaire
      , gettext "Edit with the link" appState.locale
      , gettext "Anyone on the internet with the link can edit." appState.locale
      )
    ]
