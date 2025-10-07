module Wizard.Api.Models.BootstrapConfig exposing
    ( BootstrapConfig
    , addTour
    , decoder
    , default
    )

import Common.Api.Models.AppSwitcherItem as AppSwitcherItem exposing (AppSwitcherItem)
import Common.Api.Models.UserInfo as UserInfo exposing (UserInfo)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Wizard.Api.Models.BootstrapConfig.Admin as AdminConfig exposing (Admin)
import Wizard.Api.Models.BootstrapConfig.AuthenticationConfig as AuthenticationConfig exposing (AuthenticationConfig)
import Wizard.Api.Models.BootstrapConfig.CloudConfig as CloudConfig exposing (CloudConfig)
import Wizard.Api.Models.BootstrapConfig.DashboardAndLoginScreenConfig as DashboardAndLoginScreenConfig exposing (DashboardAndLoginScreenConfig)
import Wizard.Api.Models.BootstrapConfig.FeaturesConfig as FeaturesConfig exposing (FeaturesConfig)
import Wizard.Api.Models.BootstrapConfig.LookAndFeelConfig as LookAndFeelConfig exposing (LookAndFeelConfig)
import Wizard.Api.Models.BootstrapConfig.OrganizationConfig as OrganizationConfig exposing (OrganizationConfig)
import Wizard.Api.Models.BootstrapConfig.OwlConfig as OwlConfig exposing (OwlConfig)
import Wizard.Api.Models.BootstrapConfig.PrivacyAndSupportConfig as PrivacyAndSupportConfig exposing (PrivacyAndSupportConfig)
import Wizard.Api.Models.BootstrapConfig.QuestionnaireConfig as QuestionnaireConfig exposing (QuestionnaireConfig)
import Wizard.Api.Models.BootstrapConfig.RegistryConfig as RegistryConfig exposing (RegistryConfig)
import Wizard.Api.Models.BootstrapConfig.SignalBridgeConfig as SignalBridgeConfig exposing (SignalBridgeConfig)
import Wizard.Api.Models.BootstrapConfig.SubmissionConfig as SubmissionConfig exposing (SubmissionConfig)


type alias BootstrapConfig =
    { admin : Admin
    , features : FeaturesConfig
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
    , modules : List AppSwitcherItem
    , signalBridge : SignalBridgeConfig
    , tours : List String
    , user : Maybe UserInfo
    }


default : BootstrapConfig
default =
    { admin = AdminConfig.default
    , features = FeaturesConfig.default
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
    , modules = []
    , signalBridge = SignalBridgeConfig.default
    , tours = []
    , user = Nothing
    }


decoder : Decoder BootstrapConfig
decoder =
    D.succeed BootstrapConfig
        |> D.required "admin" AdminConfig.decoder
        |> D.required "features" FeaturesConfig.decoder
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
        |> D.required "modules" (D.list AppSwitcherItem.decoder)
        |> D.required "signalBridge" SignalBridgeConfig.decoder
        |> D.required "tours" (D.list D.string)
        |> D.required "user" (D.maybe UserInfo.decoder)


addTour : String -> BootstrapConfig -> BootstrapConfig
addTour tour config =
    { config
        | tours =
            if List.member tour config.tours then
                config.tours

            else
                tour :: config.tours
    }
