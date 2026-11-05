module Dsw.Data.GuideLinks exposing (default)

{-| Where the shared pages' guide links lead in this client: the DSW guide. The
deployment's own `guideLinks` are laid over these at start-up. The guide's
version is `develop` here; the build of a release tag `vX.Y.Z` makes it `X.Y`.

The addresses are written out whole, a key and its address per line,
because the build replaces their version and a check reads them from this file.

-}

import Common.Utils.GuideLinks as GuideLinks exposing (GuideLinks)


default : GuideLinks
default =
    GuideLinks.fromList
        [ ( "documentTemplates", "https://guide.ds-wizard.org/en/develop/more/development/document-templates/index.html" )
        , ( "documentTemplatesCreate", "https://guide.ds-wizard.org/en/develop/application/document-templates/editors/create.html" )
        , ( "documentTemplatesImport", "https://guide.ds-wizard.org/en/develop/application/document-templates/list/import.html" )
        , ( "documentTemplatesPublish", "https://guide.ds-wizard.org/en/develop/application/document-templates/editors/detail/publish.html" )
        , ( "documentTemplatesSettings", "https://guide.ds-wizard.org/en/develop/application/document-templates/editors/detail/settings.html" )
        , ( "documentTemplatesUnsupportedMetamodel", "https://guide.ds-wizard.org/en/develop/more/self-hosted-dsw/faq-notes.html#document-templates-show-unsupported-metamodel-what-should-i-do" )
        , ( "jinjaCheatsheet", "https://guide.ds-wizard.org/en/develop/more/miscellaneous/jinja-cheatsheet.html" )
        , ( "kmEditorCreate", "https://guide.ds-wizard.org/en/develop/application/knowledge-models/editors/create.html" )
        , ( "kmEditorIntegrationQuestion", "https://guide.ds-wizard.org/en/develop/more/development/integration-questions/index.html" )
        , ( "kmEditorMigration", "https://guide.ds-wizard.org/en/develop/application/knowledge-models/editors/migration.html" )
        , ( "kmEditorPublish", "https://guide.ds-wizard.org/en/develop/application/knowledge-models/editors/detail/publish.html" )
        , ( "kmEditorSettings", "https://guide.ds-wizard.org/en/develop/application/knowledge-models/editors/detail/settings.html" )
        , ( "kmImport", "https://guide.ds-wizard.org/en/develop/application/knowledge-models/list/import.html" )
        , ( "markdownCheatsheet", "https://guide.ds-wizard.org/en/develop/more/miscellaneous/markdown-cheatsheet.html" )
        , ( "profileActiveSessions", "https://guide.ds-wizard.org/en/develop/application/profile/settings/active-sessions.html" )
        , ( "profileApiKeys", "https://guide.ds-wizard.org/en/develop/application/profile/settings/api-keys.html" )
        , ( "profileConnectedAccounts", "https://guide.ds-wizard.org/en/develop/application/profile/settings/connected-accounts.html" )
        , ( "profileInfo", "https://guide.ds-wizard.org/en/develop/application/profile/settings/index.html" )
        , ( "profileLanguage", "https://guide.ds-wizard.org/en/develop/application/profile/settings/language.html" )
        , ( "profilePassword", "https://guide.ds-wizard.org/en/develop/application/profile/settings/password.html" )
        , ( "profilePluginSettings", "https://guide.ds-wizard.org/en/develop/application/profile/settings/plugins.html" )
        , ( "profileSubmissionSettings", "https://guide.ds-wizard.org/en/develop/application/profile/settings/submission-settings.html" )
        , ( "profileTours", "https://guide.ds-wizard.org/en/develop/application/profile/settings/tours.html" )
        , ( "projectsCreate", "https://guide.ds-wizard.org/en/develop/application/projects/list/create.html" )
        , ( "projectsDocumentSubmission", "https://guide.ds-wizard.org/en/develop/application/projects/list/detail/documents.html#document-submission" )
        , ( "projectsDocuments", "https://guide.ds-wizard.org/en/develop/application/projects/documents.html" )
        , ( "projectsFiles", "https://guide.ds-wizard.org/en/develop/application/projects/files.html" )
        , ( "projectsMigration", "https://guide.ds-wizard.org/en/develop/application/projects/list/migration.html" )
        , ( "projectsNewDocument", "https://guide.ds-wizard.org/en/develop/application/projects/list/detail/documents.html#new-document" )
        , ( "projectsSettings", "https://guide.ds-wizard.org/en/develop/application/projects/list/detail/settings.html" )
        , ( "projectsSharing", "https://guide.ds-wizard.org/en/develop/application/projects/list/detail/sharing.html" )
        , ( "settingsAuthentication", "https://guide.ds-wizard.org/en/develop/application/administration/settings/system/authentication.html" )
        , ( "settingsDashboardAndMenu", "https://guide.ds-wizard.org/en/develop/application/administration/settings/user-interface/dashboard-and-login-screen.html" )
        , ( "settingsDocumentSubmission", "https://guide.ds-wizard.org/en/develop/application/administration/settings/content/document-submission.html" )
        , ( "settingsFeatures", "https://guide.ds-wizard.org/en/develop/application/administration/settings/system/features.html" )
        , ( "settingsLoginScreen", "https://guide.ds-wizard.org/en/develop/application/administration/settings/user-interface/login-screen.html" )
        , ( "settingsLookAndFeel", "https://guide.ds-wizard.org/en/develop/application/administration/settings/user-interface/look-and-feel.html" )
        , ( "settingsOpenId", "https://guide.ds-wizard.org/en/develop/application/administration/settings/system/openid.html" )
        , ( "settingsPlugins", "https://guide.ds-wizard.org/en/develop/application/administration/settings/system/plugins.html" )
        , ( "settingsPrivacyAndSupport", "https://guide.ds-wizard.org/en/develop/application/administration/settings/system/privacy-and-support.html" )
        , ( "settingsProjects", "https://guide.ds-wizard.org/en/develop/application/administration/settings/content/projects.html" )
        , ( "settingsRegistry", "https://guide.ds-wizard.org/en/develop/application/administration/settings/content/dsw-registry.html" )
        , ( "settingsRoles", "https://guide.ds-wizard.org/en/develop/application/administration/settings/system/roles.html" )
        , ( "settingsTranslations", "https://guide.ds-wizard.org/en/develop/application/administration/settings/translations/index.html" )
        , ( "settingsTranslationsRegistry", "https://guide.ds-wizard.org/en/develop/application/administration/settings/translations/registry.html" )
        , ( "settingsTranslationsUpload", "https://guide.ds-wizard.org/en/develop/application/administration/settings/translations/upload.html" )
        , ( "settingsUsage", "https://guide.ds-wizard.org/en/develop/application/administration/settings/info/usage.html" )
        , ( "settingsUsers", "https://guide.ds-wizard.org/en/develop/application/administration/settings/system/users.html" )
        , ( "userGroupsCreate", "https://guide.ds-wizard.org/en/develop/application/user-groups/create.html" )
        , ( "userGroupsDetail", "https://guide.ds-wizard.org/en/develop/application/user-groups/detail.html" )
        , ( "usersCreate", "https://guide.ds-wizard.org/en/develop/application/administration/users/create.html" )
        ]
