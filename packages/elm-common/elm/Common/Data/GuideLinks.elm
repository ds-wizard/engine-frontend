module Common.Data.GuideLinks exposing
    ( documentTemplates
    , documentTemplatesCreate
    , documentTemplatesImport
    , documentTemplatesPublish
    , documentTemplatesSettings
    , documentTemplatesUnsupportedMetamodel
    , jinjaCheatsheet
    , kmEditorCreate
    , kmEditorIntegrationQuestion
    , kmEditorMigration
    , kmEditorPublish
    , kmEditorSettings
    , kmImport
    , markdownCheatsheet
    , profileActiveSessions
    , profileApiKeys
    , profileLanguage
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
    , settingsLookAndFeel
    , settingsOpenId
    , settingsPlugins
    , settingsPrivacyAndSupport
    , settingsProjects
    , settingsRegistry
    , userGroupsCreate
    , userGroupsDetail
    , usersCreate
    )

import Common.Utils.GuideLinks as GuideLinks exposing (GuideLinks)


{-| The guide's pages the shared pages link to, by key. Where each key leads is
the application's to say: it puts its guide links on the app state, its own
defaults under what the deployment names.
-}
documentTemplates : GuideLinks -> String
documentTemplates =
    GuideLinks.get "documentTemplates"


documentTemplatesCreate : GuideLinks -> String
documentTemplatesCreate =
    GuideLinks.get "documentTemplatesCreate"


documentTemplatesSettings : GuideLinks -> String
documentTemplatesSettings =
    GuideLinks.get "documentTemplatesSettings"


documentTemplatesImport : GuideLinks -> String
documentTemplatesImport =
    GuideLinks.get "documentTemplatesImport"


documentTemplatesPublish : GuideLinks -> String
documentTemplatesPublish =
    GuideLinks.get "documentTemplatesPublish"


documentTemplatesUnsupportedMetamodel : GuideLinks -> String
documentTemplatesUnsupportedMetamodel =
    GuideLinks.get "documentTemplatesUnsupportedMetamodel"


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


settingsLookAndFeel : GuideLinks -> String
settingsLookAndFeel =
    GuideLinks.get "settingsLookAndFeel"


settingsOpenId : GuideLinks -> String
settingsOpenId =
    GuideLinks.get "settingsOpenId"


settingsPlugins : GuideLinks -> String
settingsPlugins =
    GuideLinks.get "settingsPlugins"


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


userGroupsCreate : GuideLinks -> String
userGroupsCreate =
    GuideLinks.get "userGroupsCreate"


userGroupsDetail : GuideLinks -> String
userGroupsDetail =
    GuideLinks.get "userGroupsDetail"
