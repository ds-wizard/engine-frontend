module Wizard.Settings.Common.EditableConfig exposing (EditableConfig, decoder, encode, updateAuthentication, updateDashboard, updateKnowledgeModelRegistry, updateLookAndFeel, updateOrganization, updatePrivacyAndSupport, updateQuestionnaires, updateSubmission)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Wizard.Common.Config.DashboardConfig as DashboardConfig exposing (DashboardConfig)
import Wizard.Common.Config.LookAndFeelConfig as LookAndFeelConfig exposing (LookAndFeelConfig)
import Wizard.Common.Config.OrganizationConfig as OrganizationConfig exposing (OrganizationConfig)
import Wizard.Common.Config.PrivacyAndSupportConfig as PrivacyAndSupportConfig exposing (PrivacyAndSupportConfig)
import Wizard.Settings.Common.EditableAuthenticationConfig as EditableAuthenticationConfig exposing (EditableAuthenticationConfig)
import Wizard.Settings.Common.EditableKnowledgeModelRegistryConfig as EditableKnowledgeModelRegistryConfig exposing (EditableKnowledgeModelRegistryConfig)
import Wizard.Settings.Common.EditableQuestionnairesConfig as EditableQuestionnairesConfig exposing (EditableQuestionnairesConfig)
import Wizard.Settings.Common.EditableSubmissionConfig as EditableSubmissionConfig exposing (EditableSubmissionConfig)


type alias EditableConfig =
    { organization : OrganizationConfig
    , authentication : EditableAuthenticationConfig
    , privacyAndSupport : PrivacyAndSupportConfig
    , dashboard : DashboardConfig
    , lookAndFeel : LookAndFeelConfig
    , knowledgeModelRegistry : EditableKnowledgeModelRegistryConfig
    , questionnaires : EditableQuestionnairesConfig
    , submission : EditableSubmissionConfig
    }


updateOrganization : OrganizationConfig -> EditableConfig -> EditableConfig
updateOrganization organization config =
    { config | organization = organization }


updateAuthentication : EditableAuthenticationConfig -> EditableConfig -> EditableConfig
updateAuthentication authentication config =
    { config | authentication = authentication }


updatePrivacyAndSupport : PrivacyAndSupportConfig -> EditableConfig -> EditableConfig
updatePrivacyAndSupport privacyAndSupport config =
    { config | privacyAndSupport = privacyAndSupport }


updateDashboard : DashboardConfig -> EditableConfig -> EditableConfig
updateDashboard dashboard config =
    { config | dashboard = dashboard }


updateLookAndFeel : LookAndFeelConfig -> EditableConfig -> EditableConfig
updateLookAndFeel lookAndFeel config =
    { config | lookAndFeel = lookAndFeel }


updateKnowledgeModelRegistry : EditableKnowledgeModelRegistryConfig -> EditableConfig -> EditableConfig
updateKnowledgeModelRegistry knowledgeModelRegistry config =
    { config | knowledgeModelRegistry = knowledgeModelRegistry }


updateQuestionnaires : EditableQuestionnairesConfig -> EditableConfig -> EditableConfig
updateQuestionnaires questionnaires config =
    { config | questionnaires = questionnaires }


updateSubmission : EditableSubmissionConfig -> EditableConfig -> EditableConfig
updateSubmission submission config =
    { config | submission = submission }



-- JSON


decoder : Decoder EditableConfig
decoder =
    D.succeed EditableConfig
        |> D.required "organization" OrganizationConfig.decoder
        |> D.required "authentication" EditableAuthenticationConfig.decoder
        |> D.required "privacyAndSupport" PrivacyAndSupportConfig.decoder
        |> D.required "dashboard" DashboardConfig.decoder
        |> D.required "lookAndFeel" LookAndFeelConfig.decoder
        |> D.required "knowledgeModelRegistry" EditableKnowledgeModelRegistryConfig.decoder
        |> D.required "questionnaire" EditableQuestionnairesConfig.decoder
        |> D.required "submission" EditableSubmissionConfig.decoder


encode : EditableConfig -> E.Value
encode config =
    E.object
        [ ( "organization", OrganizationConfig.encode config.organization )
        , ( "authentication", EditableAuthenticationConfig.encode config.authentication )
        , ( "privacyAndSupport", PrivacyAndSupportConfig.encode config.privacyAndSupport )
        , ( "dashboard", DashboardConfig.encode config.dashboard )
        , ( "lookAndFeel", LookAndFeelConfig.encode config.lookAndFeel )
        , ( "knowledgeModelRegistry", EditableKnowledgeModelRegistryConfig.encode config.knowledgeModelRegistry )
        , ( "questionnaire", EditableQuestionnairesConfig.encode config.questionnaires )
        , ( "submission", EditableSubmissionConfig.encode config.submission )
        ]
