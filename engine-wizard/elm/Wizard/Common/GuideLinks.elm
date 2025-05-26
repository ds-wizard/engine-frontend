module Wizard.Common.GuideLinks exposing
    ( GuideLinks
    , decoder
    , default
    , documentTemplates
    , documentTemplatesCreate
    , documentTemplatesImport
    , documentTemplatesPublish
    , documentTemplatesUnsupportedMetamodel
    , integrationQuestionSecrets
    , kmEditorCreate
    , kmEditorIntegrationQuestion
    , kmEditorMigration
    , kmEditorPublish
    , kmEditorSettings
    , kmImport
    , localesCreate
    , localesImport
    , markdownCheatsheet
    , merge
    , profileActiveSessions
    , profileApiKeys
    , profileLanguage
    , projectImporters
    , projectsCreate
    , projectsDocumentSubmission
    , projectsDocuments
    , projectsFiles
    , projectsMigration
    , projectsNewDocument
    , projectsSettings
    , projectsSharing
    , settingsAuthentication
    , settingsDashboardAndLoginScreen
    , settingsDocumentSubmission
    , settingsKnowledgeModels
    , settingsLookAndFeel
    , settingsOrganization
    , settingsPrivacyAndSupport
    , settingsProjects
    , settingsRegistry
    , usersCreate
    , wrap
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api.ExternalLink as ExternalLink


type GuideLinks
    = GuideLinks (Dict String String)


decoder : Decoder GuideLinks
decoder =
    D.dict D.string
        |> D.map GuideLinks


default : GuideLinks
default =
    GuideLinks <|
        Dict.fromList
            [ ( "documentTemplates", "https://guide.ds-wizard.org/en/latest/more/development/document-templates/index.html" )
            , ( "documentTemplatesCreate", "https://guide.ds-wizard.org/en/latest/application/document-templates/editors/create.html" )
            , ( "documentTemplatesImport", "https://guide.ds-wizard.org/en/latest/application/document-templates/list/import.html" )
            , ( "documentTemplatesPublish", "https://guide.ds-wizard.org/en/latest/application/document-templates/editors/detail/publish.html" )
            , ( "documentTemplatesUnsupportedMetamodel", "https://guide.ds-wizard.org/en/latest/more/self-hosted-dsw/faq-notes.html#document-templates-show-unsupported-metamodel-what-should-i-do" )
            , ( "integrationQuestionSecrets", "https://guide.ds-wizard.org/en/latest/more/development/integration-questions/integration-api.html#secrets-and-other-properties" )
            , ( "kmEditorCreate", "https://guide.ds-wizard.org/en/latest/application/knowledge-models/editors/create.html" )
            , ( "kmEditorIntegrationQuestion", "https://guide.ds-wizard.org/en/latest/more/development/integration-questions/index.html" )
            , ( "kmEditorMigration", "https://guide.ds-wizard.org/en/latest/application/knowledge-models/editors/migration.html" )
            , ( "kmEditorPublish", "https://guide.ds-wizard.org/en/latest/application/knowledge-models/editors/detail/publish.html" )
            , ( "kmEditorSettings", "https://guide.ds-wizard.org/en/latest/application/knowledge-models/editors/detail/settings.html" )
            , ( "kmImport", "https://guide.ds-wizard.org/en/latest/application/knowledge-models/list/import.html" )
            , ( "localesCreate", "https://guide.ds-wizard.org/en/latest/application/administration/locales/create.html" )
            , ( "localesImport", "https://guide.ds-wizard.org/en/latest/application/administration/locales/import.html" )
            , ( "markdownCheatsheet", "https://guide.ds-wizard.org/en/latest/more/miscellaneous/markdown-cheatsheet.html" )
            , ( "profileActiveSessions", "https://guide.ds-wizard.org/en/latest/application/profile/settings/active-sessions.html" )
            , ( "profileApiKeys", "https://guide.ds-wizard.org/en/latest/application/profile/settings/api-keys.html" )
            , ( "profileLanguage", "https://guide.ds-wizard.org/en/latest/application/profile/settings/language.html" )
            , ( "projectsCreate", "https://guide.ds-wizard.org/en/latest/application/projects/list/create.html" )
            , ( "projectsDocumentSubmission", "https://guide.ds-wizard.org/en/latest/application/projects/list/detail/documents.html#document-submission" )
            , ( "projectsDocuments", "https://guide.ds-wizard.org/en/latest/application/projects/documents.html" )
            , ( "projectsImporters", "https://guide.ds-wizard.org/en/latest/application/projects/importers.html" )
            , ( "projectsFiles", "https://guide.ds-wizard.org/en/latest/application/projects/files.html" )
            , ( "projectsMigration", "https://guide.ds-wizard.org/en/latest/application/projects/list/migration.html" )
            , ( "projectsNewDocument", "https://guide.ds-wizard.org/en/latest/application/projects/list/detail/documents.html#new-document" )
            , ( "projectsSettings", "https://guide.ds-wizard.org/en/latest/application/projects/list/detail/settings.html" )
            , ( "projectsSharing", "https://guide.ds-wizard.org/en/latest/application/projects/list/detail/sharing.html" )
            , ( "settingsAuthentication", "https://guide.ds-wizard.org/en/latest/application/administration/settings/system/authentication.html" )
            , ( "settingsDashboardAndLoginScreen", "https://guide.ds-wizard.org/en/latest/application/administration/settings/user-interface/dashboard-and-login-screen.html" )
            , ( "settingsDocumentSubmission", "https://guide.ds-wizard.org/en/latest/application/administration/settings/content/document-submission.html" )
            , ( "settingsKnowledgeModels", "https://guide.ds-wizard.org/en/latest/application/administration/settings/content/knowledge-models.html" )
            , ( "settingsLookAndFeel", "https://guide.ds-wizard.org/en/latest/application/administration/settings/user-interface/look-and-feel.html" )
            , ( "settingsOrganization", "https://guide.ds-wizard.org/en/latest/application/administration/settings/system/organization.html" )
            , ( "settingsPrivacyAndSupport", "https://guide.ds-wizard.org/en/latest/application/administration/settings/system/privacy-and-support.html" )
            , ( "settingsProjects", "https://guide.ds-wizard.org/en/latest/application/administration/settings/content/projects.html" )
            , ( "settingsRegistry", "https://guide.ds-wizard.org/en/latest/application/administration/settings/content/dsw-registry.html" )
            , ( "usersCreate", "https://guide.ds-wizard.org/en/latest/application/administration/users/create.html" )
            ]


merge : GuideLinks -> GuideLinks -> GuideLinks
merge (GuideLinks guideLinksA) (GuideLinks guideLinksB) =
    GuideLinks <|
        Dict.merge
            (\key a -> Dict.insert key a)
            (\key a _ -> Dict.insert key a)
            (\key b -> Dict.insert key b)
            guideLinksA
            guideLinksB
            Dict.empty


wrap : AbstractAppState a -> String -> String
wrap =
    ExternalLink.externalLinkUrl


get : String -> GuideLinks -> String
get key (GuideLinks guideLinks) =
    Dict.get key guideLinks
        |> Maybe.withDefault ""


documentTemplates : GuideLinks -> String
documentTemplates =
    get "documentTemplates"


documentTemplatesCreate : GuideLinks -> String
documentTemplatesCreate =
    get "documentTemplatesCreate"


documentTemplatesImport : GuideLinks -> String
documentTemplatesImport =
    get "documentTemplatesImport"


documentTemplatesPublish : GuideLinks -> String
documentTemplatesPublish =
    get "documentTemplatesPublish"


documentTemplatesUnsupportedMetamodel : GuideLinks -> String
documentTemplatesUnsupportedMetamodel =
    get "documentTemplatesUnsupportedMetamodel"


integrationQuestionSecrets : GuideLinks -> String
integrationQuestionSecrets =
    get "integrationQuestionSecrets"


kmEditorCreate : GuideLinks -> String
kmEditorCreate =
    get "kmEditorCreate"


kmEditorIntegrationQuestion : GuideLinks -> String
kmEditorIntegrationQuestion =
    get "kmEditorIntegrationQuestion"


kmEditorMigration : GuideLinks -> String
kmEditorMigration =
    get "kmEditorMigration"


kmEditorPublish : GuideLinks -> String
kmEditorPublish =
    get "kmEditorPublish"


kmEditorSettings : GuideLinks -> String
kmEditorSettings =
    get "kmEditorSettings"


kmImport : GuideLinks -> String
kmImport =
    get "kmImport"


localesCreate : GuideLinks -> String
localesCreate =
    get "localesCreate"


localesImport : GuideLinks -> String
localesImport =
    get "localesImport"


markdownCheatsheet : GuideLinks -> String
markdownCheatsheet =
    get "markdownCheatsheet"


profileLanguage : GuideLinks -> String
profileLanguage =
    get "profileLanguage"


profileApiKeys : GuideLinks -> String
profileApiKeys =
    get "profileApiKeys"


profileActiveSessions : GuideLinks -> String
profileActiveSessions =
    get "profileActiveSessions"


projectsCreate : GuideLinks -> String
projectsCreate =
    get "projectsCreate"


projectsDocumentSubmission : GuideLinks -> String
projectsDocumentSubmission =
    get "projectsDocumentSubmission"


projectsDocuments : GuideLinks -> String
projectsDocuments =
    get "projectsDocuments"


projectsFiles : GuideLinks -> String
projectsFiles =
    get "projectsFiles"


projectImporters : GuideLinks -> String
projectImporters =
    get "projectsImporters"


projectsMigration : GuideLinks -> String
projectsMigration =
    get "projectsMigration"


projectsNewDocument : GuideLinks -> String
projectsNewDocument =
    get "projectsNewDocument"


projectsSettings : GuideLinks -> String
projectsSettings =
    get "projectsSettings"


projectsSharing : GuideLinks -> String
projectsSharing =
    get "projectsSharing"


settingsAuthentication : GuideLinks -> String
settingsAuthentication =
    get "settingsAuthentication"


settingsDashboardAndLoginScreen : GuideLinks -> String
settingsDashboardAndLoginScreen =
    get "settingsDashboardAndLoginScreen"


settingsDocumentSubmission : GuideLinks -> String
settingsDocumentSubmission =
    get "settingsDocumentSubmission"


settingsKnowledgeModels : GuideLinks -> String
settingsKnowledgeModels =
    get "settingsKnowledgeModels"


settingsLookAndFeel : GuideLinks -> String
settingsLookAndFeel =
    get "settingsLookAndFeel"


settingsOrganization : GuideLinks -> String
settingsOrganization =
    get "settingsOrganization"


settingsPrivacyAndSupport : GuideLinks -> String
settingsPrivacyAndSupport =
    get "settingsPrivacyAndSupport"


settingsProjects : GuideLinks -> String
settingsProjects =
    get "settingsProjects"


settingsRegistry : GuideLinks -> String
settingsRegistry =
    get "settingsRegistry"


usersCreate : GuideLinks -> String
usersCreate =
    get "usersCreate"
