module Shared.Data.BootstrapConfig exposing
    ( BootstrapConfig
    , decoder
    , default
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Data.BootstrapConfig.AuthenticationConfig as AuthenticationConfig exposing (AuthenticationConfig)
import Shared.Data.BootstrapConfig.DashboardConfig as DashboardConfig exposing (DashboardConfig)
import Shared.Data.BootstrapConfig.KnowledgeModelRegistryConfig as KnowledgeModelRegistryConfig exposing (KnowledgeModelRegistryConfig)
import Shared.Data.BootstrapConfig.LookAndFeelConfig as LookAndFeelConfig exposing (LookAndFeelConfig)
import Shared.Data.BootstrapConfig.OrganizationConfig as OrganizationConfig exposing (OrganizationConfig)
import Shared.Data.BootstrapConfig.PrivacyAndSupportConfig as PrivacyAndSupportConfig exposing (PrivacyAndSupportConfig)
import Shared.Data.BootstrapConfig.QuestionnaireConfig as QuestionnaireConfig exposing (QuestionnaireConfig)
import Shared.Data.BootstrapConfig.SubmissionConfig as SubmissionConfig exposing (SubmissionConfig)
import Shared.Data.BootstrapConfig.TemplateConfig as TemplateConfig exposing (TemplateConfig)


type alias BootstrapConfig =
    { authentication : AuthenticationConfig
    , dashboard : DashboardConfig
    , knowledgeModelRegistry : KnowledgeModelRegistryConfig
    , lookAndFeel : LookAndFeelConfig
    , organization : OrganizationConfig
    , privacyAndSupport : PrivacyAndSupportConfig
    , questionnaire : QuestionnaireConfig
    , submission : SubmissionConfig
    , template : TemplateConfig
    }


default : BootstrapConfig
default =
    { authentication = AuthenticationConfig.default
    , dashboard = DashboardConfig.default
    , knowledgeModelRegistry = KnowledgeModelRegistryConfig.default
    , lookAndFeel = LookAndFeelConfig.default
    , organization = OrganizationConfig.default
    , privacyAndSupport = PrivacyAndSupportConfig.default
    , questionnaire = QuestionnaireConfig.default
    , submission = SubmissionConfig.default
    , template = TemplateConfig.default
    }


decoder : Decoder BootstrapConfig
decoder =
    D.succeed BootstrapConfig
        |> D.required "authentication" AuthenticationConfig.decoder
        |> D.required "dashboard" DashboardConfig.decoder
        |> D.required "knowledgeModelRegistry" KnowledgeModelRegistryConfig.decoder
        |> D.required "lookAndFeel" LookAndFeelConfig.decoder
        |> D.required "organization" OrganizationConfig.decoder
        |> D.required "privacyAndSupport" PrivacyAndSupportConfig.decoder
        |> D.required "questionnaire" QuestionnaireConfig.decoder
        |> D.required "submission" SubmissionConfig.decoder
        |> D.required "template" TemplateConfig.decoder
