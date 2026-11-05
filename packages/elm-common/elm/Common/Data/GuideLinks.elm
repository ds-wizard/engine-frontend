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
    , profileConnectedAccounts
    , profileInfo
    , profileLanguage
    , profilePassword
    , profilePluginSettings
    , profileSubmissionSettings
    , profileTours
    , projectsCreate
    , projectsDocumentSubmission
    , projectsDocuments
    , projectsFiles
    , projectsMigration
    , projectsNewDocument
    , projectsSettings
    , projectsSharing
    , settingsAuthentication
    , settingsDashboardAndMenu
    , settingsDocumentSubmission
    , settingsFeatures
    , settingsLoginScreen
    , settingsLookAndFeel
    , settingsOpenId
    , settingsPlugins
    , settingsPrivacyAndSupport
    , settingsProjects
    , settingsRegistry
    , settingsRoles
    , settingsTranslations
    , settingsTranslationsRegistry
    , settingsTranslationsUpload
    , settingsUsage
    , settingsUsers
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


documentTemplatesImport : GuideLinks -> String
documentTemplatesImport =
    GuideLinks.get "documentTemplatesImport"


documentTemplatesPublish : GuideLinks -> String
documentTemplatesPublish =
    GuideLinks.get "documentTemplatesPublish"


documentTemplatesSettings : GuideLinks -> String
documentTemplatesSettings =
    GuideLinks.get "documentTemplatesSettings"


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


profileActiveSessions : GuideLinks -> String
profileActiveSessions =
    GuideLinks.get "profileActiveSessions"


profileApiKeys : GuideLinks -> String
profileApiKeys =
    GuideLinks.get "profileApiKeys"


profileConnectedAccounts : GuideLinks -> String
profileConnectedAccounts =
    GuideLinks.get "profileConnectedAccounts"


profileInfo : GuideLinks -> String
profileInfo =
    GuideLinks.get "profileInfo"


profileLanguage : GuideLinks -> String
profileLanguage =
    GuideLinks.get "profileLanguage"


profilePassword : GuideLinks -> String
profilePassword =
    GuideLinks.get "profilePassword"


profilePluginSettings : GuideLinks -> String
profilePluginSettings =
    GuideLinks.get "profilePluginSettings"


profileSubmissionSettings : GuideLinks -> String
profileSubmissionSettings =
    GuideLinks.get "profileSubmissionSettings"


profileTours : GuideLinks -> String
profileTours =
    GuideLinks.get "profileTours"


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


settingsDashboardAndMenu : GuideLinks -> String
settingsDashboardAndMenu =
    GuideLinks.get "settingsDashboardAndMenu"


settingsDocumentSubmission : GuideLinks -> String
settingsDocumentSubmission =
    GuideLinks.get "settingsDocumentSubmission"


settingsFeatures : GuideLinks -> String
settingsFeatures =
    GuideLinks.get "settingsFeatures"


settingsLoginScreen : GuideLinks -> String
settingsLoginScreen =
    GuideLinks.get "settingsLoginScreen"


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


settingsRoles : GuideLinks -> String
settingsRoles =
    GuideLinks.get "settingsRoles"


settingsTranslations : GuideLinks -> String
settingsTranslations =
    GuideLinks.get "settingsTranslations"


settingsTranslationsRegistry : GuideLinks -> String
settingsTranslationsRegistry =
    GuideLinks.get "settingsTranslationsRegistry"


settingsTranslationsUpload : GuideLinks -> String
settingsTranslationsUpload =
    GuideLinks.get "settingsTranslationsUpload"


settingsUsage : GuideLinks -> String
settingsUsage =
    GuideLinks.get "settingsUsage"


settingsUsers : GuideLinks -> String
settingsUsers =
    GuideLinks.get "settingsUsers"


userGroupsCreate : GuideLinks -> String
userGroupsCreate =
    GuideLinks.get "userGroupsCreate"


userGroupsDetail : GuideLinks -> String
userGroupsDetail =
    GuideLinks.get "userGroupsDetail"


usersCreate : GuideLinks -> String
usersCreate =
    GuideLinks.get "usersCreate"
