module Wizard.Common.Provisioning.DefaultLocale exposing (locale)

import Dict exposing (Dict)


locale : Dict String String
locale =
    Dict.fromList
        [ -- Routing
          ( "__routing.dashboard", "dashboard" )
        , ( "__routing.documents", "project-documents" )
        , ( "__routing.documents.index.questionnaireUuid", "questionnaireUuid" )
        , ( "__routing.documentTemplateEditors", "document-template-editors" )
        , ( "__routing.kmEditor", "km-editor" )
        , ( "__routing.kmEditor.create", "create" )
        , ( "__routing.kmEditor.create.selected", "selected" )
        , ( "__routing.kmEditor.create.edit", "edit" )
        , ( "__routing.kmEditor.editor", "editor" )
        , ( "__routing.kmEditor.migration", "migration" )
        , ( "__routing.kmEditor.publish", "publish" )
        , ( "__routing.knowledgeModels", "knowledge-models" )
        , ( "__routing.knowledgeModels.import", "import" )
        , ( "__routing.knowledgeModels.import.packageId", "packageId" )
        , ( "__routing.knowledgeModels.preview", "preview" )
        , ( "__routing.knowledgeModels.preview.questionUuid", "questionUuid" )
        , ( "__routing.login.originalUrl", "originalUrl" )
        , ( "__routing.locales", "locales" )
        , ( "__routing.locales.create", "create" )
        , ( "__routing.locales.import", "import" )
        , ( "__routing.locales.import.localeId", "localeId" )
        , ( "__routing.projectActions", "project-actions" )
        , ( "__routing.projectFiles", "project-files" )
        , ( "__routing.projectImporters", "project-importers" )
        , ( "__routing.projects", "projects" )
        , ( "__routing.projects.create", "create" )
        , ( "__routing.projects.create.selectedProjectTemplate", "selectedProjectTemplate" )
        , ( "__routing.projects.create.selectedKnowledgeModel", "selectedKnowledgeModel" )
        , ( "__routing.projects.createMigration", "create-migration" )
        , ( "__routing.projects.migration", "migration" )
        , ( "__routing.public.forgottenPassword", "forgotten-password" )
        , ( "__routing.public.signup", "signup" )
        , ( "__routing.registry", "registry" )
        , ( "__routing.registry.signupConfirmation", "signup" )
        , ( "__routing.settings", "settings" )
        , ( "__routing.settings.authentication", "authentication" )
        , ( "__routing.settings.dashboard", "dashboard" )
        , ( "__routing.settings.registry", "registry" )
        , ( "__routing.settings.knowledgeModel", "knowledge-models" )
        , ( "__routing.settings.lookAndFeel", "look-and-feel" )
        , ( "__routing.settings.organization", "organization" )
        , ( "__routing.settings.privacyAndSupport", "privacy-and-support" )
        , ( "__routing.settings.projects", "projects" )
        , ( "__routing.settings.submission", "submission" )
        , ( "__routing.settings.usage", "usage" )
        , ( "__routing.documentTemplates", "document-templates" )
        , ( "__routing.documentTemplates.import", "import" )
        , ( "__routing.documentTemplates.import.documentTemplateId", "documentTemplateId" )
        ]
