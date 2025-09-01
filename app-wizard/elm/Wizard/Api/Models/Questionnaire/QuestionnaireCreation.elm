module Wizard.Api.Models.Questionnaire.QuestionnaireCreation exposing
    ( QuestionnaireCreation(..)
    , customEnabled
    , decoder
    , encode
    , field
    , fromTemplateEnabled
    , richFormOptions
    , validation
    )

import Form.Error as Error exposing (ErrorValue(..))
import Form.Field as Field exposing (Field)
import Form.Validate as V exposing (Validation)
import Gettext exposing (gettext)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E


type QuestionnaireCreation
    = CustomQuestionnaireCreation
    | TemplateQuestionnaireCreation
    | TemplateAndCustomQuestionnaireCreation


toString : QuestionnaireCreation -> String
toString questionnaireCreation =
    case questionnaireCreation of
        CustomQuestionnaireCreation ->
            "CustomQuestionnaireCreation"

        TemplateQuestionnaireCreation ->
            "TemplateQuestionnaireCreation"

        TemplateAndCustomQuestionnaireCreation ->
            "TemplateAndCustomQuestionnaireCreation"


fromString : String -> Maybe QuestionnaireCreation
fromString str =
    case str of
        "CustomQuestionnaireCreation" ->
            Just CustomQuestionnaireCreation

        "TemplateQuestionnaireCreation" ->
            Just TemplateQuestionnaireCreation

        "TemplateAndCustomQuestionnaireCreation" ->
            Just TemplateAndCustomQuestionnaireCreation

        _ ->
            Nothing


encode : QuestionnaireCreation -> E.Value
encode =
    E.string << toString


decoder : Decoder QuestionnaireCreation
decoder =
    D.string
        |> D.andThen
            (\str ->
                case fromString str of
                    Just value ->
                        D.succeed value

                    Nothing ->
                        D.fail <| "Unknown questionnaire creation: " ++ str
            )


customEnabled : QuestionnaireCreation -> Bool
customEnabled questionnaireCreation =
    case questionnaireCreation of
        TemplateQuestionnaireCreation ->
            False

        _ ->
            True


fromTemplateEnabled : QuestionnaireCreation -> Bool
fromTemplateEnabled questionnaireCreation =
    case questionnaireCreation of
        CustomQuestionnaireCreation ->
            False

        _ ->
            True


field : QuestionnaireCreation -> Field
field =
    toString >> Field.string


validation : Validation e QuestionnaireCreation
validation =
    V.string
        |> V.andThen
            (\value ->
                case fromString value of
                    Just v ->
                        V.succeed v

                    Nothing ->
                        V.fail (Error.value InvalidString)
            )


richFormOptions : { a | locale : Gettext.Locale } -> List ( String, String, String )
richFormOptions appState =
    [ ( toString TemplateAndCustomQuestionnaireCreation
      , gettext "Templates & Custom" appState.locale
      , gettext "Projects can be created from project templates or with custom settings." appState.locale
      )
    , ( toString TemplateQuestionnaireCreation
      , gettext "Templates only" appState.locale
      , gettext "Only project templates can be used for creating new projects." appState.locale
      )
    , ( toString CustomQuestionnaireCreation
      , gettext "Custom only" appState.locale
      , gettext "Project templates feature is disabled, researchers have to choose knowledge model and set up everything." appState.locale
      )
    ]
