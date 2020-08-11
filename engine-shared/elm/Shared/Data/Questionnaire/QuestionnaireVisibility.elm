module Shared.Data.Questionnaire.QuestionnaireVisibility exposing
    ( QuestionnaireVisibility(..)
    , decoder
    , encode
    , field
    , formOptions
    , fromFormValues
    , fromString
    , richFormOptions
    , toFormValues
    , toString
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


type QuestionnaireVisibility
    = PrivateQuestionnaire
    | VisibleViewQuestionnaire
    | VisibleEditQuestionnaire


toString : QuestionnaireVisibility -> String
toString questionnaireVisibility =
    case questionnaireVisibility of
        PrivateQuestionnaire ->
            "PrivateQuestionnaire"

        VisibleViewQuestionnaire ->
            "VisibleViewQuestionnaire"

        VisibleEditQuestionnaire ->
            "VisibleEditQuestionnaire"


fromString : String -> Maybe QuestionnaireVisibility
fromString str =
    case str of
        "PrivateQuestionnaire" ->
            Just PrivateQuestionnaire

        "VisibleViewQuestionnaire" ->
            Just VisibleViewQuestionnaire

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

        VisibleEditQuestionnaire ->
            ( True, QuestionnairePermission.Edit )


fromFormValues : Bool -> QuestionnairePermission -> Bool -> QuestionnairePermission -> QuestionnaireVisibility
fromFormValues enabled perm sharingEnabled sharingPerm =
    if enabled then
        if perm == QuestionnairePermission.Edit || (sharingEnabled && sharingPerm == QuestionnairePermission.Edit) then
            VisibleEditQuestionnaire

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

                    "VisibleEditQuestionnaire" ->
                        Validate.succeed VisibleEditQuestionnaire

                    _ ->
                        Validate.fail <| Error.value InvalidString
            )


richFormOptions : { a | provisioning : Provisioning } -> List ( String, String, String )
richFormOptions appState =
    [ ( toString PrivateQuestionnaire
      , lg "questionnaireVisibility.private" appState
      , lg "questionnaireVisibility.private.description" appState
      )
    , ( toString VisibleViewQuestionnaire
      , lg "questionnaireVisibility.publicReadOnly" appState
      , lg "questionnaireVisibility.publicReadOnly.description" appState
      )
    , ( toString VisibleEditQuestionnaire
      , lg "questionnaireVisibility.public" appState
      , lg "questionnaireVisibility.public.description" appState
      )
    ]


formOptions : { a | provisioning : Provisioning } -> List ( String, String )
formOptions appState =
    [ ( toString PrivateQuestionnaire
      , lg "questionnaireVisibility.private" appState
      )
    , ( toString VisibleViewQuestionnaire
      , lg "questionnaireVisibility.publicReadOnly" appState
      )
    , ( toString VisibleEditQuestionnaire
      , lg "questionnaireVisibility.public" appState
      )
    ]
