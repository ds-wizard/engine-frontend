module Wizard.Common.Config exposing
    ( Config
    , decoder
    , default
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Wizard.Common.Config.AuthenticationConfig as AuthenticationConfig exposing (AuthenticationConfig)
import Wizard.Common.Config.DashboardConfig as DashboardConfig exposing (DashboardConfig)
import Wizard.Common.Config.KnowledgeModelRegistryConfig as KnowledgeModelRegistryConfig exposing (KnowledgeModelRegistryConfig)
import Wizard.Common.Config.LookAndFeelConfig as LookAndFeelConfig exposing (LookAndFeelConfig)
import Wizard.Common.Config.OrganizationConfig as OrganizationConfig exposing (OrganizationConfig)
import Wizard.Common.Config.PrivacyAndSupportConfig as PrivacyAndSupportConfig exposing (PrivacyAndSupportConfig)
import Wizard.Common.Config.QuestionnairesConfig as QuestionnairesConfig exposing (QuestionnairesConfig)


type alias Config =
    { organization : OrganizationConfig
    , authentication : AuthenticationConfig
    , privacyAndSupport : PrivacyAndSupportConfig
    , dashboard : DashboardConfig
    , lookAndFeel : LookAndFeelConfig
    , knowledgeModelRegistry : KnowledgeModelRegistryConfig
    , questionnaires : QuestionnairesConfig
    }


decoder : Decoder Config
decoder =
    D.succeed Config
        |> D.required "organization" OrganizationConfig.decoder
        |> D.required "authentication" AuthenticationConfig.decoder
        |> D.required "privacyAndSupport" PrivacyAndSupportConfig.decoder
        |> D.required "dashboard" DashboardConfig.decoder
        |> D.required "lookAndFeel" LookAndFeelConfig.decoder
        |> D.required "knowledgeModelRegistry" KnowledgeModelRegistryConfig.decoder
        |> D.required "questionnaire" QuestionnairesConfig.decoder


default : Config
default =
    { organization = OrganizationConfig.default
    , authentication = AuthenticationConfig.default
    , privacyAndSupport = PrivacyAndSupportConfig.default
    , dashboard = DashboardConfig.default
    , lookAndFeel = LookAndFeelConfig.default
    , knowledgeModelRegistry = KnowledgeModelRegistryConfig.default
    , questionnaires = QuestionnairesConfig.default
    }
