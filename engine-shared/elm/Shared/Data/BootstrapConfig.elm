module Shared.Data.BootstrapConfig exposing
    ( BootstrapConfig
    , decoder
    , default
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Data.BootstrapConfig.AuthenticationConfig as AuthenticationConfig exposing (AuthenticationConfig)
import Shared.Data.BootstrapConfig.CloudConfig as CloudConfig exposing (CloudConfig)
import Shared.Data.BootstrapConfig.DashboardConfig as DashboardConfig exposing (DashboardConfig)
import Shared.Data.BootstrapConfig.FeatureConfig as FeatureConfig exposing (FeatureConfig)
import Shared.Data.BootstrapConfig.LookAndFeelConfig as LookAndFeelConfig exposing (LookAndFeelConfig)
import Shared.Data.BootstrapConfig.OrganizationConfig as OrganizationConfig exposing (OrganizationConfig)
import Shared.Data.BootstrapConfig.OwlConfig as OwlConfig exposing (OwlConfig)
import Shared.Data.BootstrapConfig.PrivacyAndSupportConfig as PrivacyAndSupportConfig exposing (PrivacyAndSupportConfig)
import Shared.Data.BootstrapConfig.QuestionnaireConfig as QuestionnaireConfig exposing (QuestionnaireConfig)
import Shared.Data.BootstrapConfig.RegistryConfig as RegistryConfig exposing (RegistryConfig)
import Shared.Data.BootstrapConfig.SubmissionConfig as SubmissionConfig exposing (SubmissionConfig)
import Shared.Data.BootstrapConfig.TemplateConfig as TemplateConfig exposing (TemplateConfig)


type alias BootstrapConfig =
    { authentication : AuthenticationConfig
    , dashboard : DashboardConfig
    , registry : RegistryConfig
    , lookAndFeel : LookAndFeelConfig
    , organization : OrganizationConfig
    , privacyAndSupport : PrivacyAndSupportConfig
    , questionnaire : QuestionnaireConfig
    , submission : SubmissionConfig
    , template : TemplateConfig
    , feature : FeatureConfig
    , cloud : CloudConfig
    , owl : OwlConfig
    }


default : BootstrapConfig
default =
    { authentication = AuthenticationConfig.default
    , dashboard = DashboardConfig.default
    , registry = RegistryConfig.default
    , lookAndFeel = LookAndFeelConfig.default
    , organization = OrganizationConfig.default
    , privacyAndSupport = PrivacyAndSupportConfig.default
    , questionnaire = QuestionnaireConfig.default
    , submission = SubmissionConfig.default
    , template = TemplateConfig.default
    , feature = FeatureConfig.default
    , cloud = CloudConfig.default
    , owl = OwlConfig.default
    }


decoder : Decoder BootstrapConfig
decoder =
    D.succeed BootstrapConfig
        |> D.required "authentication" AuthenticationConfig.decoder
        |> D.required "dashboard" DashboardConfig.decoder
        |> D.required "registry" RegistryConfig.decoder
        |> D.required "lookAndFeel" LookAndFeelConfig.decoder
        |> D.required "organization" OrganizationConfig.decoder
        |> D.required "privacyAndSupport" PrivacyAndSupportConfig.decoder
        |> D.required "questionnaire" QuestionnaireConfig.decoder
        |> D.required "submission" SubmissionConfig.decoder
        |> D.required "template" TemplateConfig.decoder
        |> D.required "feature" FeatureConfig.decoder
        |> D.required "cloud" CloudConfig.decoder
        |> D.required "owl" OwlConfig.decoder
