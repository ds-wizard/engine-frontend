module Shared.Data.Questionnaire.QuestionnaireCreation exposing
    ( QuestionnaireCreation(..)
    , customEnabled
    , decoder
    , encode
    , field
    , fromString
    , fromTemplateEnabled
    , richFormOptions
    , toString
    , validation
    )

import Form.Error as Error exposing (ErrorValue(..))
import Form.Field as Field exposing (Field)
import Form.Validate as V exposing (Validation)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Shared.Locale exposing (lg)
import Shared.Provisioning exposing (Provisioning)


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


richFormOptions : { a | provisioning : Provisioning } -> List ( String, String, String )
richFormOptions appState =
    [ ( toString TemplateAndCustomQuestionnaireCreation
      , lg "questionnaireCreation.templateAndCustom" appState
      , lg "questionnaireCreation.templateAndCustom.description" appState
      )
    , ( toString TemplateQuestionnaireCreation
      , lg "questionnaireCreation.template" appState
      , lg "questionnaireCreation.template.description" appState
      )
    , ( toString CustomQuestionnaireCreation
      , lg "questionnaireCreation.custom" appState
      , lg "questionnaireCreation.custom.description" appState
      )
    ]
