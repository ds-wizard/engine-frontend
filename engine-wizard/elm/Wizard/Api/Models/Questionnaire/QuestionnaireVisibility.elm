module Wizard.Api.Models.Questionnaire.QuestionnaireVisibility exposing
    ( QuestionnaireVisibility(..)
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


type QuestionnaireVisibility
    = PrivateQuestionnaire
    | VisibleViewQuestionnaire
    | VisibleCommentQuestionnaire
    | VisibleEditQuestionnaire


toString : QuestionnaireVisibility -> String
toString questionnaireVisibility =
    case questionnaireVisibility of
        PrivateQuestionnaire ->
            "PrivateQuestionnaire"

        VisibleViewQuestionnaire ->
            "VisibleViewQuestionnaire"

        VisibleCommentQuestionnaire ->
            "VisibleCommentQuestionnaire"

        VisibleEditQuestionnaire ->
            "VisibleEditQuestionnaire"


fromString : String -> Maybe QuestionnaireVisibility
fromString str =
    case str of
        "PrivateQuestionnaire" ->
            Just PrivateQuestionnaire

        "VisibleViewQuestionnaire" ->
            Just VisibleViewQuestionnaire

        "VisibleCommentQuestionnaire" ->
            Just VisibleCommentQuestionnaire

        "VisibleEditQuestionnaire" ->
            Just VisibleEditQuestionnaire

        _ ->
            Nothing


encode : QuestionnaireVisibility -> E.Value
encode =
    E.string << toString


decoder : Decoder QuestionnaireVisibility
decoder =
    D.string
        |> D.andThen
            (\str ->
                case fromString str of
                    Just visibility ->
                        D.succeed visibility

                    Nothing ->
                        D.fail <| "Unknown questionnaire visibility: " ++ str
            )


toFormValues : QuestionnaireVisibility -> ( Bool, QuestionnairePermission )
toFormValues sharing =
    case sharing of
        PrivateQuestionnaire ->
            ( False, QuestionnairePermission.View )

        VisibleViewQuestionnaire ->
            ( True, QuestionnairePermission.View )

        VisibleCommentQuestionnaire ->
            ( True, QuestionnairePermission.Comment )

        VisibleEditQuestionnaire ->
            ( True, QuestionnairePermission.Edit )


fromFormValues : Bool -> QuestionnairePermission -> Bool -> QuestionnairePermission -> QuestionnaireVisibility
fromFormValues enabled perm sharingEnabled sharingPerm =
    if enabled then
        if perm == QuestionnairePermission.Edit || (sharingEnabled && sharingPerm == QuestionnairePermission.Edit) then
            VisibleEditQuestionnaire

        else if perm == QuestionnairePermission.Comment || (sharingEnabled && sharingPerm == QuestionnairePermission.Comment) then
            VisibleCommentQuestionnaire

        else
            VisibleViewQuestionnaire

    else
        PrivateQuestionnaire


field : QuestionnaireVisibility -> Field
field =
    toString >> Field.string


validation : Validation e QuestionnaireVisibility
validation =
    Validate.string
        |> Validate.andThen
            (\valueType ->
                case valueType of
                    "PrivateQuestionnaire" ->
                        Validate.succeed PrivateQuestionnaire

                    "VisibleViewQuestionnaire" ->
                        Validate.succeed VisibleViewQuestionnaire

                    "VisibleCommentQuestionnaire" ->
                        Validate.succeed VisibleCommentQuestionnaire

                    "VisibleEditQuestionnaire" ->
                        Validate.succeed VisibleEditQuestionnaire

                    _ ->
                        Validate.fail <| Error.value InvalidString
            )


richFormOptions : { a | locale : Gettext.Locale } -> List ( String, String, String )
richFormOptions appState =
    [ ( toString PrivateQuestionnaire
      , gettext "Private" appState.locale
      , gettext "Visible only to the owner and invited users." appState.locale
      )
    , ( toString VisibleViewQuestionnaire
      , gettext "Visible - View" appState.locale
      , gettext "Other logged-in users can view the project." appState.locale
      )
    , ( toString VisibleCommentQuestionnaire
      , gettext "Visible - Comment" appState.locale
      , gettext "Other logged-in users can view and comment the project." appState.locale
      )
    , ( toString VisibleEditQuestionnaire
      , gettext "Visible - Edit" appState.locale
      , gettext "Other logged-in users can edit the project." appState.locale
      )
    ]
