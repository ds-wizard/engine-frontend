module Shared.Data.BootstrapConfig.QuestionnaireConfig.QuestionnaireVisibilityConfig exposing
    ( QuestionnaireVisibilityConfig
    , decoder
    , default
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Data.Questionnaire.QuestionnaireVisibility as QuestionnaireVisibility exposing (QuestionnaireVisibility)


type alias QuestionnaireVisibilityConfig =
    { enabled : Bool
    , defaultValue : QuestionnaireVisibility
    }


decoder : Decoder QuestionnaireVisibilityConfig
decoder =
    D.succeed QuestionnaireVisibilityConfig
        |> D.required "enabled" D.bool
        |> D.required "defaultValue" QuestionnaireVisibility.decoder


default : QuestionnaireVisibilityConfig
default =
    { enabled = True
    , defaultValue = QuestionnaireVisibility.PrivateQuestionnaire
    }
