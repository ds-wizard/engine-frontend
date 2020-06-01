module Shared.Data.BootstrapConfig exposing
    ( BootstrapConfig
    , decoder
    , default
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Data.BootstrapConfig.AuthenticationConfig as AuthenticationConfig exposing (AuthenticationConfig)
import Shared.Data.BootstrapConfig.LookAndFeelConfig as LookAndFeelConfig exposing (LookAndFeelConfig)
import Shared.Data.BootstrapConfig.QuestionnaireConfig as QuestionnaireConfig exposing (QuestionnaireConfig)
import Shared.Data.BootstrapConfig.TemplateConfig as TemplateConfig exposing (TemplateConfig)


type alias BootstrapConfig =
    { authentication : AuthenticationConfig
    , lookAndFeel : LookAndFeelConfig
    , questionnaire : QuestionnaireConfig
    , template : TemplateConfig
    }


default : BootstrapConfig
default =
    { authentication = AuthenticationConfig.default
    , lookAndFeel = LookAndFeelConfig.default
    , questionnaire = QuestionnaireConfig.default
    , template = TemplateConfig.default
    }


decoder : Decoder BootstrapConfig
decoder =
    D.succeed BootstrapConfig
        |> D.required "authentication" AuthenticationConfig.decoder
        |> D.required "lookAndFeel" LookAndFeelConfig.decoder
        |> D.required "questionnaire" QuestionnaireConfig.decoder
        |> D.required "template" TemplateConfig.decoder
