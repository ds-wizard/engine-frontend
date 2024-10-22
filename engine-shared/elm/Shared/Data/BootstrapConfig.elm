module Shared.Data.BootstrapConfig exposing
    ( BootstrapConfig
    , decoder
    , default
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Data.BootstrapConfig.AIAssistantConfig as AIAssistantConfig exposing (AIAssistantConfig)
import Shared.Data.BootstrapConfig.Admin as AdminConfig exposing (Admin)
import Shared.Data.BootstrapConfig.AppSwitcherItem as AppSwitcherItem exposing (AppSwitcherItem)
import Shared.Data.BootstrapConfig.AuthenticationConfig as AuthenticationConfig exposing (AuthenticationConfig)
import Shared.Data.BootstrapConfig.CloudConfig as CloudConfig exposing (CloudConfig)
import Shared.Data.BootstrapConfig.DashboardAndLoginScreenConfig as DashboardAndLoginScreenConfig exposing (DashboardAndLoginScreenConfig)
import Shared.Data.BootstrapConfig.LocaleConfig as LocaleConfig exposing (LocaleConfig)
import Shared.Data.BootstrapConfig.LookAndFeelConfig as LookAndFeelConfig exposing (LookAndFeelConfig)
import Shared.Data.BootstrapConfig.OrganizationConfig as OrganizationConfig exposing (OrganizationConfig)
import Shared.Data.BootstrapConfig.OwlConfig as OwlConfig exposing (OwlConfig)
import Shared.Data.BootstrapConfig.PrivacyAndSupportConfig as PrivacyAndSupportConfig exposing (PrivacyAndSupportConfig)
import Shared.Data.BootstrapConfig.QuestionnaireConfig as QuestionnaireConfig exposing (QuestionnaireConfig)
import Shared.Data.BootstrapConfig.RegistryConfig as RegistryConfig exposing (RegistryConfig)
import Shared.Data.BootstrapConfig.SignalBridgeConfig as SignalBridgeConfig exposing (SignalBridgeConfig)
import Shared.Data.BootstrapConfig.SubmissionConfig as SubmissionConfig exposing (SubmissionConfig)
import Shared.Data.UserInfo as UserInfo exposing (UserInfo)


type alias BootstrapConfig =
    { admin : Admin
    , aiAssistant : AIAssistantConfig
    , authentication : AuthenticationConfig
    , dashboardAndLoginScreen : DashboardAndLoginScreenConfig
    , registry : RegistryConfig
    , lookAndFeel : LookAndFeelConfig
    , organization : OrganizationConfig
    , privacyAndSupport : PrivacyAndSupportConfig
    , questionnaire : QuestionnaireConfig
    , submission : SubmissionConfig
    , cloud : CloudConfig
    , owl : OwlConfig
    , locales : List LocaleConfig
    , modules : List AppSwitcherItem
    , signalBridge : SignalBridgeConfig
    , user : Maybe UserInfo
    }


default : BootstrapConfig
default =
    { admin = AdminConfig.default
    , aiAssistant = AIAssistantConfig.default
    , authentication = AuthenticationConfig.default
    , dashboardAndLoginScreen = DashboardAndLoginScreenConfig.default
    , registry = RegistryConfig.default
    , lookAndFeel = LookAndFeelConfig.default
    , organization = OrganizationConfig.default
    , privacyAndSupport = PrivacyAndSupportConfig.default
    , questionnaire = QuestionnaireConfig.default
    , submission = SubmissionConfig.default
    , cloud = CloudConfig.default
    , owl = OwlConfig.default
    , locales = []
    , modules = []
    , signalBridge = SignalBridgeConfig.default
    , user = Nothing
    }


decoder : Decoder BootstrapConfig
decoder =
    D.succeed BootstrapConfig
        |> D.required "admin" AdminConfig.decoder
        |> D.required "aiAssistant" AIAssistantConfig.decoder
        |> D.required "authentication" AuthenticationConfig.decoder
        |> D.required "dashboardAndLoginScreen" DashboardAndLoginScreenConfig.decoder
        |> D.required "registry" RegistryConfig.decoder
        |> D.required "lookAndFeel" LookAndFeelConfig.decoder
        |> D.required "organization" OrganizationConfig.decoder
        |> D.required "privacyAndSupport" PrivacyAndSupportConfig.decoder
        |> D.required "questionnaire" QuestionnaireConfig.decoder
        |> D.required "submission" SubmissionConfig.decoder
        |> D.required "cloud" CloudConfig.decoder
        |> D.required "owl" OwlConfig.decoder
        |> D.required "locales" (D.list LocaleConfig.decoder)
        |> D.required "modules" (D.list AppSwitcherItem.decoder)
        |> D.required "signalBridge" SignalBridgeConfig.decoder
        |> D.required "user" (D.maybe UserInfo.decoder)
