module Wizard.Common.Config.Partials.QuestionnaireVisibilityConfig exposing
    ( QuestionnaireVisibilityConfig
    , decoder
    , default
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Wizard.Questionnaires.Common.QuestionnaireVisibility as QuestionnaireVisibility exposing (QuestionnaireVisibility)


type alias QuestionnaireVisibilityConfig =
    { enabled : Bool
    , defaultValue : QuestionnaireVisibility
    }


decoder : Decoder QuestionnaireVisibilityConfig
decoder =
    D.succeed QuestionnaireVisibilityConfig
        |> D.required "enabled" D.bool
        |> D.required "defaultValue" QuestionnaireVisibility.decoder


encode : QuestionnaireVisibilityConfig -> E.Value
encode config =
    E.object
        [ ( "enabled", E.bool config.enabled )
        , ( "defaultValue", QuestionnaireVisibility.encode config.defaultValue )
        ]


default : QuestionnaireVisibilityConfig
default =
    { enabled = True
    , defaultValue = QuestionnaireVisibility.PrivateQuestionnaire
    }
