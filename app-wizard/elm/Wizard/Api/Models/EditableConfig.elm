module Wizard.Api.Models.EditableConfig exposing
    ( EditableConfig
    , decoder
    , encode
    , updateAuthentication
    , updateDashboardAndLoginScreen
    , updateFeatures
    , updateKnowledgeModel
    , updateLookAndFeel
    , updateOrganization
    , updatePrivacyAndSupport
    , updateQuestionnaires
    , updateRegistry
    , updateSubmission
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Wizard.Api.Models.BootstrapConfig.DashboardAndLoginScreenConfig as DashboardAndLoginScreenConfig exposing (DashboardAndLoginScreenConfig)
import Wizard.Api.Models.BootstrapConfig.OrganizationConfig as OrganizationConfig exposing (OrganizationConfig)
import Wizard.Api.Models.BootstrapConfig.PrivacyAndSupportConfig as PrivacyAndSupportConfig exposing (PrivacyAndSupportConfig)
import Wizard.Api.Models.EditableConfig.EditableAuthenticationConfig as EditableAuthenticationConfig exposing (EditableAuthenticationConfig)
import Wizard.Api.Models.EditableConfig.EditableFeaturesConfig as EditableFeaturesConfig exposing (EditableFeaturesConfig)
import Wizard.Api.Models.EditableConfig.EditableKnowledgeModelConfig as EditableKnowledgeModelConfig exposing (EditableKnowledgeModelConfig)
import Wizard.Api.Models.EditableConfig.EditableLookAndFeelConfig as EditableLookAndFeelConfig exposing (EditableLookAndFeelConfig)
import Wizard.Api.Models.EditableConfig.EditableQuestionnairesConfig as EditableQuestionnairesConfig exposing (EditableQuestionnairesConfig)
import Wizard.Api.Models.EditableConfig.EditableRegistryConfig as EditableRegistryConfig exposing (EditableRegistryConfig)
import Wizard.Api.Models.EditableConfig.EditableSubmissionConfig as EditableSubmissionConfig exposing (EditableSubmissionConfig)


type alias EditableConfig =
    { organization : OrganizationConfig
    , authentication : EditableAuthenticationConfig
    , privacyAndSupport : PrivacyAndSupportConfig
    , features : EditableFeaturesConfig
    , dashboardAndLoginScreen : DashboardAndLoginScreenConfig
    , lookAndFeel : EditableLookAndFeelConfig
    , registry : EditableRegistryConfig
    , questionnaires : EditableQuestionnairesConfig
    , submission : EditableSubmissionConfig
    , knowledgeModel : EditableKnowledgeModelConfig
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


updateFeatures : EditableFeaturesConfig -> EditableConfig -> EditableConfig
updateFeatures features config =
    { config | features = features }


updateDashboardAndLoginScreen : DashboardAndLoginScreenConfig -> EditableConfig -> EditableConfig
updateDashboardAndLoginScreen dashboard config =
    { config | dashboardAndLoginScreen = dashboard }


updateLookAndFeel : EditableLookAndFeelConfig -> EditableConfig -> EditableConfig
updateLookAndFeel lookAndFeel config =
    { config | lookAndFeel = lookAndFeel }


updateRegistry : EditableRegistryConfig -> EditableConfig -> EditableConfig
updateRegistry registry config =
    { config | registry = registry }


updateQuestionnaires : EditableQuestionnairesConfig -> EditableConfig -> EditableConfig
updateQuestionnaires questionnaires config =
    { config | questionnaires = questionnaires }


updateSubmission : EditableSubmissionConfig -> EditableConfig -> EditableConfig
updateSubmission submission config =
    { config | submission = submission }


updateKnowledgeModel : EditableKnowledgeModelConfig -> EditableConfig -> EditableConfig
updateKnowledgeModel knowledgeModel config =
    { config | knowledgeModel = knowledgeModel }



-- JSON


decoder : Decoder EditableConfig
decoder =
    D.succeed EditableConfig
        |> D.required "organization" OrganizationConfig.decoder
        |> D.required "authentication" EditableAuthenticationConfig.decoder
        |> D.required "privacyAndSupport" PrivacyAndSupportConfig.decoder
        |> D.required "features" EditableFeaturesConfig.decoder
        |> D.required "dashboardAndLoginScreen" DashboardAndLoginScreenConfig.decoder
        |> D.required "lookAndFeel" EditableLookAndFeelConfig.decoder
        |> D.required "registry" EditableRegistryConfig.decoder
        |> D.required "questionnaire" EditableQuestionnairesConfig.decoder
        |> D.required "submission" EditableSubmissionConfig.decoder
        |> D.required "knowledgeModel" EditableKnowledgeModelConfig.decoder


encode : EditableConfig -> E.Value
encode config =
    E.object
        [ ( "organization", OrganizationConfig.encode config.organization )
        , ( "authentication", EditableAuthenticationConfig.encode config.authentication )
        , ( "privacyAndSupport", PrivacyAndSupportConfig.encode config.privacyAndSupport )
        , ( "features", EditableFeaturesConfig.encode config.features )
        , ( "dashboardAndLoginScreen", DashboardAndLoginScreenConfig.encode config.dashboardAndLoginScreen )
        , ( "lookAndFeel", EditableLookAndFeelConfig.encode config.lookAndFeel )
        , ( "registry", EditableRegistryConfig.encode config.registry )
        , ( "questionnaire", EditableQuestionnairesConfig.encode config.questionnaires )
        , ( "submission", EditableSubmissionConfig.encode config.submission )
        , ( "knowledgeModel", EditableKnowledgeModelConfig.encode config.knowledgeModel )
        ]
