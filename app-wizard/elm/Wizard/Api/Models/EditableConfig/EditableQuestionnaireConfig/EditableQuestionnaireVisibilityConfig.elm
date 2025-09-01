module Wizard.Api.Models.EditableConfig.EditableQuestionnaireConfig.EditableQuestionnaireVisibilityConfig exposing
    ( EditableQuestionnaireVisibilityConfig
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Wizard.Api.Models.Questionnaire.QuestionnaireVisibility as QuestionnaireVisibility exposing (QuestionnaireVisibility)


type alias EditableQuestionnaireVisibilityConfig =
    { enabled : Bool
    , defaultValue : QuestionnaireVisibility
    }


decoder : Decoder EditableQuestionnaireVisibilityConfig
decoder =
    D.succeed EditableQuestionnaireVisibilityConfig
        |> D.required "enabled" D.bool
        |> D.required "defaultValue" QuestionnaireVisibility.decoder


encode : EditableQuestionnaireVisibilityConfig -> E.Value
encode config =
    E.object
        [ ( "enabled", E.bool config.enabled )
        , ( "defaultValue", QuestionnaireVisibility.encode config.defaultValue )
        ]
