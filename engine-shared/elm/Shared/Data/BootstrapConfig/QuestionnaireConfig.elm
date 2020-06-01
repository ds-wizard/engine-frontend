module Shared.Data.BootstrapConfig.QuestionnaireConfig exposing
    ( QuestionnaireConfig
    , decoder
    , default
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Data.BootstrapConfig.QuestionnaireConfig.QuestionnaireVisibilityConfig as QuestionnaireVisibilityConfig exposing (QuestionnaireVisibilityConfig)


type alias QuestionnaireConfig =
    { questionnaireVisibility : QuestionnaireVisibilityConfig
    }


decoder : Decoder QuestionnaireConfig
decoder =
    D.succeed QuestionnaireConfig
        |> D.required "questionnaireVisibility" QuestionnaireVisibilityConfig.decoder


default : QuestionnaireConfig
default =
    { questionnaireVisibility = QuestionnaireVisibilityConfig.default
    }
