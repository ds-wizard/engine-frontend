module Shared.Data.EditableConfig exposing
    ( EditableConfig
    , decoder
    , encode
    , updateAuthentication
    , updateDashboard
    , updateLookAndFeel
    , updateOrganization
    , updatePrivacyAndSupport
    , updateQuestionnaires
    , updateRegistry
    , updateSubmission
    , updateTemplate
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Shared.Data.BootstrapConfig.DashboardConfig as DashboardConfig exposing (DashboardConfig)
import Shared.Data.BootstrapConfig.LookAndFeelConfig as LookAndFeelConfig exposing (LookAndFeelConfig)
import Shared.Data.BootstrapConfig.OrganizationConfig as OrganizationConfig exposing (OrganizationConfig)
import Shared.Data.BootstrapConfig.PrivacyAndSupportConfig as PrivacyAndSupportConfig exposing (PrivacyAndSupportConfig)
import Shared.Data.BootstrapConfig.TemplateConfig as TemplateConfig exposing (TemplateConfig)
import Shared.Data.EditableConfig.EditableAuthenticationConfig as EditableAuthenticationConfig exposing (EditableAuthenticationConfig)
import Shared.Data.EditableConfig.EditableQuestionnairesConfig as EditableQuestionnairesConfig exposing (EditableQuestionnairesConfig)
import Shared.Data.EditableConfig.EditableRegistryConfig as EditableRegistryConfig exposing (EditableRegistryConfig)
import Shared.Data.EditableConfig.EditableSubmissionConfig as EditableSubmissionConfig exposing (EditableSubmissionConfig)


type alias EditableConfig =
    { organization : OrganizationConfig
    , authentication : EditableAuthenticationConfig
    , privacyAndSupport : PrivacyAndSupportConfig
    , dashboard : DashboardConfig
    , lookAndFeel : LookAndFeelConfig
    , registry : EditableRegistryConfig
    , questionnaires : EditableQuestionnairesConfig
    , submission : EditableSubmissionConfig
    , template : TemplateConfig
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


updateRegistry : EditableRegistryConfig -> EditableConfig -> EditableConfig
updateRegistry registry config =
    { config | registry = registry }


updateQuestionnaires : EditableQuestionnairesConfig -> EditableConfig -> EditableConfig
updateQuestionnaires questionnaires config =
    { config | questionnaires = questionnaires }


updateSubmission : EditableSubmissionConfig -> EditableConfig -> EditableConfig
updateSubmission submission config =
    { config | submission = submission }


updateTemplate : TemplateConfig -> EditableConfig -> EditableConfig
updateTemplate template config =
    { config | template = template }



-- JSON


decoder : Decoder EditableConfig
decoder =
    D.succeed EditableConfig
        |> D.required "organization" OrganizationConfig.decoder
        |> D.required "authentication" EditableAuthenticationConfig.decoder
        |> D.required "privacyAndSupport" PrivacyAndSupportConfig.decoder
        |> D.required "dashboard" DashboardConfig.decoder
        |> D.required "lookAndFeel" LookAndFeelConfig.decoder
        |> D.required "registry" EditableRegistryConfig.decoder
        |> D.required "questionnaire" EditableQuestionnairesConfig.decoder
        |> D.required "submission" EditableSubmissionConfig.decoder
        |> D.required "template" TemplateConfig.decoder


encode : EditableConfig -> E.Value
encode config =
    E.object
        [ ( "organization", OrganizationConfig.encode config.organization )
        , ( "authentication", EditableAuthenticationConfig.encode config.authentication )
        , ( "privacyAndSupport", PrivacyAndSupportConfig.encode config.privacyAndSupport )
        , ( "dashboard", DashboardConfig.encode config.dashboard )
        , ( "lookAndFeel", LookAndFeelConfig.encode config.lookAndFeel )
        , ( "registry", EditableRegistryConfig.encode config.registry )
        , ( "questionnaire", EditableQuestionnairesConfig.encode config.questionnaires )
        , ( "submission", EditableSubmissionConfig.encode config.submission )
        , ( "template", TemplateConfig.encode config.template )
        ]
