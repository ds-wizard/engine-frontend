module Wizard.Utils.WizardGuideLinks exposing
    ( default
    , documentTemplates
    , documentTemplatesCreate
    , documentTemplatesImport
    , documentTemplatesPublish
    , documentTemplatesUnsupportedMetamodel
    , integrationQuestionSecrets
    , jinjaCheatsheet
    , kmEditorCreate
    , kmEditorIntegrationQuestion
    , kmEditorMigration
    , kmEditorPublish
    , kmEditorSettings
    , kmImport
    , localesCreate
    , localesImport
    , markdownCheatsheet
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
    , settingsFeatures
    , settingsKnowledgeModels
    , settingsLookAndFeel
    , settingsOrganization
    , settingsPrivacyAndSupport
    , settingsProjects
    , settingsRegistry
    , usersCreate
    )

import Common.Utils.GuideLinks as GuideLinks exposing (GuideLinks)


default : GuideLinks
default =
    GuideLinks.fromList
        [ ( "documentTemplates", "https://guide.ds-wizard.org/en/latest/more/development/document-templates/index.html" )
        , ( "documentTemplatesCreate", "https://guide.ds-wizard.org/en/latest/application/document-templates/editors/create.html" )
        , ( "documentTemplatesImport", "https://guide.ds-wizard.org/en/latest/application/document-templates/list/import.html" )
        , ( "documentTemplatesPublish", "https://guide.ds-wizard.org/en/latest/application/document-templates/editors/detail/publish.html" )
        , ( "documentTemplatesUnsupportedMetamodel", "https://guide.ds-wizard.org/en/latest/more/self-hosted-dsw/faq-notes.html#document-templates-show-unsupported-metamodel-what-should-i-do" )
        , ( "integrationQuestionSecrets", "https://guide.ds-wizard.org/en/latest/more/development/integration-questions/integration-api.html#secrets-and-other-properties" )
        , ( "jinjaCheatsheet", "https://guide.ds-wizard.org/en/latest/more/miscellaneous/jinja-cheatsheet.html" )
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
        , ( "settingsFeatures", "https://guide.ds-wizard.org/en/latest/application/administration/settings/system/features.html" )
        , ( "settingsKnowledgeModels", "https://guide.ds-wizard.org/en/latest/application/administration/settings/content/knowledge-models.html" )
        , ( "settingsLookAndFeel", "https://guide.ds-wizard.org/en/latest/application/administration/settings/user-interface/look-and-feel.html" )
        , ( "settingsOrganization", "https://guide.ds-wizard.org/en/latest/application/administration/settings/system/organization.html" )
        , ( "settingsPrivacyAndSupport", "https://guide.ds-wizard.org/en/latest/application/administration/settings/system/privacy-and-support.html" )
        , ( "settingsProjects", "https://guide.ds-wizard.org/en/latest/application/administration/settings/content/projects.html" )
        , ( "settingsRegistry", "https://guide.ds-wizard.org/en/latest/application/administration/settings/content/dsw-registry.html" )
        , ( "usersCreate", "https://guide.ds-wizard.org/en/latest/application/administration/users/create.html" )
        ]


documentTemplates : GuideLinks -> String
documentTemplates =
    GuideLinks.get "documentTemplates"


documentTemplatesCreate : GuideLinks -> String
documentTemplatesCreate =
    GuideLinks.get "documentTemplatesCreate"


documentTemplatesImport : GuideLinks -> String
documentTemplatesImport =
    GuideLinks.get "documentTemplatesImport"


documentTemplatesPublish : GuideLinks -> String
documentTemplatesPublish =
    GuideLinks.get "documentTemplatesPublish"


documentTemplatesUnsupportedMetamodel : GuideLinks -> String
documentTemplatesUnsupportedMetamodel =
    GuideLinks.get "documentTemplatesUnsupportedMetamodel"


integrationQuestionSecrets : GuideLinks -> String
integrationQuestionSecrets =
    GuideLinks.get "integrationQuestionSecrets"


jinjaCheatsheet : GuideLinks -> String
jinjaCheatsheet =
    GuideLinks.get "jinjaCheatsheet"


kmEditorCreate : GuideLinks -> String
kmEditorCreate =
    GuideLinks.get "kmEditorCreate"


kmEditorIntegrationQuestion : GuideLinks -> String
kmEditorIntegrationQuestion =
    GuideLinks.get "kmEditorIntegrationQuestion"


kmEditorMigration : GuideLinks -> String
kmEditorMigration =
    GuideLinks.get "kmEditorMigration"


kmEditorPublish : GuideLinks -> String
kmEditorPublish =
    GuideLinks.get "kmEditorPublish"


kmEditorSettings : GuideLinks -> String
kmEditorSettings =
    GuideLinks.get "kmEditorSettings"


kmImport : GuideLinks -> String
kmImport =
    GuideLinks.get "kmImport"


localesCreate : GuideLinks -> String
localesCreate =
    GuideLinks.get "localesCreate"


localesImport : GuideLinks -> String
localesImport =
    GuideLinks.get "localesImport"


markdownCheatsheet : GuideLinks -> String
markdownCheatsheet =
    GuideLinks.get "markdownCheatsheet"


profileLanguage : GuideLinks -> String
profileLanguage =
    GuideLinks.get "profileLanguage"


profileApiKeys : GuideLinks -> String
profileApiKeys =
    GuideLinks.get "profileApiKeys"


profileActiveSessions : GuideLinks -> String
profileActiveSessions =
    GuideLinks.get "profileActiveSessions"


projectsCreate : GuideLinks -> String
projectsCreate =
    GuideLinks.get "projectsCreate"


projectsDocumentSubmission : GuideLinks -> String
projectsDocumentSubmission =
    GuideLinks.get "projectsDocumentSubmission"


projectsDocuments : GuideLinks -> String
projectsDocuments =
    GuideLinks.get "projectsDocuments"


projectsFiles : GuideLinks -> String
projectsFiles =
    GuideLinks.get "projectsFiles"


projectImporters : GuideLinks -> String
projectImporters =
    GuideLinks.get "projectsImporters"


projectsMigration : GuideLinks -> String
projectsMigration =
    GuideLinks.get "projectsMigration"


projectsNewDocument : GuideLinks -> String
projectsNewDocument =
    GuideLinks.get "projectsNewDocument"


projectsSettings : GuideLinks -> String
projectsSettings =
    GuideLinks.get "projectsSettings"


projectsSharing : GuideLinks -> String
projectsSharing =
    GuideLinks.get "projectsSharing"


settingsAuthentication : GuideLinks -> String
settingsAuthentication =
    GuideLinks.get "settingsAuthentication"


settingsDashboardAndLoginScreen : GuideLinks -> String
settingsDashboardAndLoginScreen =
    GuideLinks.get "settingsDashboardAndLoginScreen"


settingsDocumentSubmission : GuideLinks -> String
settingsDocumentSubmission =
    GuideLinks.get "settingsDocumentSubmission"


settingsFeatures : GuideLinks -> String
settingsFeatures =
    GuideLinks.get "settingsFeatures"


settingsKnowledgeModels : GuideLinks -> String
settingsKnowledgeModels =
    GuideLinks.get "settingsKnowledgeModels"


settingsLookAndFeel : GuideLinks -> String
settingsLookAndFeel =
    GuideLinks.get "settingsLookAndFeel"


settingsOrganization : GuideLinks -> String
settingsOrganization =
    GuideLinks.get "settingsOrganization"


settingsPrivacyAndSupport : GuideLinks -> String
settingsPrivacyAndSupport =
    GuideLinks.get "settingsPrivacyAndSupport"


settingsProjects : GuideLinks -> String
settingsProjects =
    GuideLinks.get "settingsProjects"


settingsRegistry : GuideLinks -> String
settingsRegistry =
    GuideLinks.get "settingsRegistry"


usersCreate : GuideLinks -> String
usersCreate =
    GuideLinks.get "usersCreate"
