module Wizard.Common.Provisioning.DefaultIconSet exposing (iconSet)

import Dict exposing (Dict)


iconSet : Dict String String
iconSet =
    Dict.fromList
        [ ( "_global.add", "fas fa-plus" )
        , ( "_global.arrowRight", "fas fa-long-arrow-alt-right" )
        , ( "_global.cancel", "fas fa-ban" )
        , ( "_global.chevronLeft", "fas fa-chevron-left" )
        , ( "_global.chevronRight", "fas fa-chevron-right" )
        , ( "_global.close", "fas fa-times" )
        , ( "_global.copy", "fas fa-paste" )
        , ( "_global.delete", "fas fa-trash" )
        , ( "_global.disable", "fas fa-toggle-off" )
        , ( "_global.download", "fas fa-download" )
        , ( "_global.edit", "fas fa-edit" )
        , ( "_global.enable", "fas fa-toggle-on" )
        , ( "_global.error", "fas fa-exclamation-circle" )
        , ( "_global.export", "fas fa-download" )
        , ( "_global.externalLink", "fas fa-external-link-alt" )
        , ( "_global.info", "fas fa-info-circle" )
        , ( "_global.knowledgeModel", "fas fa-sitemap" )
        , ( "_global.open", "far fa-folder-open" )
        , ( "_global.preview", "fas fa-eye" )
        , ( "_global.questionnaire", "far fa-list-alt" )
        , ( "_global.remove", "fas fa-times" )
        , ( "_global.secretShow", "fas fa-fw fa-eye" )
        , ( "_global.secretHide", "fas fa-fw fa-eye-slash" )
        , ( "_global.settings", "fas fa-cogs" )
        , ( "_global.sortAsc", "fas fa-arrow-down-long" )
        , ( "_global.sortDesc", "fas fa-arrow-up-long" )
        , ( "_global.spinner", "fas fa-spinner fa-spin" )
        , ( "_global.success", "fas fa-check" )
        , ( "_global.view", "far fa-eye" )
        , ( "_global.warning", "fas fa-exclamation-triangle" )
        , ( "activeSession.revoke", "fas fa-ban" )
        , ( "avatar.0", "fas fa-cat" )
        , ( "avatar.1", "fas fa-crow" )
        , ( "avatar.2", "fas fa-dog" )
        , ( "avatar.3", "fas fa-dove" )
        , ( "avatar.4", "fas fa-dragon" )
        , ( "avatar.5", "fas fa-fish" )
        , ( "avatar.6", "fas fa-frog" )
        , ( "avatar.7", "fas fa-hippo" )
        , ( "avatar.8", "fas fa-horse" )
        , ( "avatar.9", "fas fa-kiwi-bird" )
        , ( "avatar.10", "fas fa-otter" )
        , ( "avatar.11", "fas fa-spider" )
        , ( "avatar.12", "fas fa-piggy-bank" )
        , ( "avatar.13", "fas fa-bug" )
        , ( "avatar.14", "fas fa-hat-wizard" )
        , ( "avatar.15", "fas fa-ghost" )
        , ( "avatar.16", "fas fa-robot" )
        , ( "avatar.17", "fas fa-snowman" )
        , ( "avatar.18", "fas fa-tree" )
        , ( "avatar.19", "fas fa-hat-cowboy" )
        , ( "detail.showAll", "fas fa-angle-down" )
        , ( "documents.download", "fas fa-download" )
        , ( "documents.viewError", "fas fa-exclamation-circle" )
        , ( "documents.submit", "fas fa-paper-plane" )
        , ( "documentTemplate.restore", "fas fa-undo-alt" )
        , ( "documentTemplate.setDeprecated", "fas fa-ban" )
        , ( "documentTemplateEditor.files", "far fa-file-code" )
        , ( "documentTemplateEditor.publish", "fas fa-cloud-upload-alt" )
        , ( "import.file", "far fa-file" )
        , ( "km.answer", "far fa-dot-circle" )
        , ( "km.choice", "far fa-check-square" )
        , ( "km.chapter", "far fa-file" )
        , ( "km.expert", "far fa-user" )
        , ( "km.fork", "fas fa-code-branch" )
        , ( "km.integration", "fas fa-exchange-alt" )
        , ( "km.itemTemplate", "far fa-file-alt" )
        , ( "km.knowledgeModel", "fas fa-database" )
        , ( "km.metric", "fas fa-chart-column" )
        , ( "km.phase", "far fa-clock" )
        , ( "km.question", "far fa-comment" )
        , ( "km.reference", "far fa-bookmark" )
        , ( "km.resourceCollection", "fas fa-book" )
        , ( "km.resourcePage", "far fa-file-lines" )
        , ( "km.tag", "fas fa-tag" )
        , ( "kmEditor.collapseAll", "fas fa-angle-double-up" )
        , ( "kmEditor.copyUuid", "fas fa-paste" )
        , ( "kmEditor.expandAll", "fas fa-angle-double-down" )
        , ( "kmEditor.knowledgeModel", "fas fa-sitemap" )
        , ( "kmEditor.move", "fas fa-reply" )
        , ( "kmEditor.tags", "fas fa-tags" )
        , ( "kmEditor.treeOpened", "fas fa-caret-down" )
        , ( "kmEditor.treeClosed", "fas fa-caret-right" )
        , ( "kmEditorList.continueMigration", "fas fa-long-arrow-alt-right" )
        , ( "kmEditorList.edit", "fas fa-pen" )
        , ( "kmEditorList.edited", "fas fa-pen" )
        , ( "kmEditorList.publish", "fas fa-cloud-upload-alt" )
        , ( "kmEditorList.update", "fas fa-sort-amount-up" )
        , ( "kmDetail.createKMEditor", "fas fa-edit" )
        , ( "kmDetail.createQuestionnaire", "far fa-list-alt" )
        , ( "kmDetail.fork", "fas fa-code-branch" )
        , ( "kmDetail.registryLink", "fas fa-external-link-alt" )
        , ( "kmImport.fromFile", "fas fa-upload" )
        , ( "kmImport.fromOwl", "fas fa-project-diagram" )
        , ( "kmImport.fromRegistry", "fas fa-cloud-download-alt" )
        , ( "kms.upload", "fas fa-upload" )
        , ( "listing.actions", "fas fa-ellipsis-v" )
        , ( "listing.filter.multi.selected", "fas fa-check-square" )
        , ( "listing.filter.multi.notSelected", "far fa-square" )
        , ( "listing.filter.single.selected", "fas fa-check-circle" )
        , ( "listing.filter.single.notSelected", "far fa-circle" )
        , ( "locale.create", "fas fa-plus" )
        , ( "locale.import", "fas fa-upload" )
        , ( "locale.default", "fas fa-check-circle" )
        , ( "locale.selected", "fas fa-check" )
        , ( "login.externalService", "fab fa-openid" )
        , ( "menu.about", "fas fa-info" )
        , ( "menu.administration", "fas fa-cog" )
        , ( "menu.assignedComments", "fas fa-check-to-slot" )
        , ( "menu.collapse", "fas fa-angle-double-left" )
        , ( "menu.dashboard", "fas fa-home" )
        , ( "menu.dev", "fas fa-laptop-code" )
        , ( "menu.documents", "fas fa-layer-group" )
        , ( "menu.knowledgeModels", "fas fa-sitemap" )
        , ( "menu.language", "fas fa-language" )
        , ( "menu.logout", "fas fa-sign-out-alt" )
        , ( "menu.open", "fas fa-angle-double-right" )
        , ( "menu.profile", "fas fa-user-edit" )
        , ( "menu.projects", "fas fa-folder" )
        , ( "menu.reportIssue", "fas fa-exclamation-triangle" )
        , ( "menu.templates", "fas fa-file-code" )
        , ( "menu.tenants", "fas fa-server" )
        , ( "persistentCommand.retry", "fas fa-sync-alt" )
        , ( "project.documents", "fa far fa-copy" )
        , ( "project.files", "fa far fa-folder-closed" )
        , ( "project.metrics", "fa far fa-chart-bar" )
        , ( "project.questionnaire", "fa far fa-list-alt" )
        , ( "project.sharing.private", "fa-fw fa-lock" )
        , ( "project.sharing.internal", "fa-fw fa-user-friends" )
        , ( "project.sharing.public", "fa-fw fa-globe" )
        , ( "questionnaire.answeredIndication", "fas fa-check" )
        , ( "questionnaire.clearAnswer", "fas fa-undo-alt" )
        , ( "questionnaire.comments", "fas fa-comments" )
        , ( "questionnaire.commentsAssign", "fas fa-fw fa-user-plus" )
        , ( "questionnaire.commentsResolve", "fas fa-fw fa-check" )
        , ( "questionnaire.copyLink", "fas fa-fw fa-link" )
        , ( "questionnaire.copyLinkCopied", "fas fa-fw fa-check" )
        , ( "questionnaire.desirable", "far fa-check-square" )
        , ( "questionnaire.expand", "fas fa-expand" )
        , ( "questionnaire.expand", "fas fa-expand" )
        , ( "questionnaire.experts", "far fa-address-book" )
        , ( "questionnaire.feedback", "fas fa-exclamation" )
        , ( "questionnaire.followUpsIndication", "fas fa-list-ul" )
        , ( "questionnaire.history.createDocument", "far fa-file" )
        , ( "questionnaire.history.revert", "fas fa-history" )
        , ( "questionnaire.item.collapse", "fa-fw fas fa-chevron-up" )
        , ( "questionnaire.item.collapseAll", "fas fa-angle-double-up" )
        , ( "questionnaire.item.expand", "fa-fw fas fa-chevron-down" )
        , ( "questionnaire.item.expandAll", "fas fa-angle-double-down" )
        , ( "questionnaire.item.moveUp", "fa-fw fas fa-arrow-up" )
        , ( "questionnaire.item.moveDown", "fa-fw fas fa-arrow-down" )
        , ( "questionnaire.resourcePageReferences", "fas fa-book" )
        , ( "questionnaire.saving.saving", "fas fa-sync-alt fa-spin" )
        , ( "questionnaire.saving.saved", "far fa-check-circle" )
        , ( "questionnaire.settings.kmAllQuestions", "far fa-check-square" )
        , ( "questionnaire.settings.kmFiltered", "fas fa-filter" )
        , ( "questionnaire.shrink", "fas fa-compress" )
        , ( "questionnaire.urlReferences", "fas fa-external-link-alt" )
        , ( "questionnaireList.clone", "far fa-copy" )
        , ( "questionnaireList.createMigration", "fas fa-random" )
        , ( "questionnaireList.createProjectFromTemplate", "far fa-list-alt" )
        , ( "questionnaireMigration.resolve", "fas fa-check" )
        , ( "questionnaireMigration.resolveAll", "fas fa-check-double" )
        , ( "questionnaireMigration.undo", "fas fa-undo-alt" )
        , ( "userAgent.desktop", "fas fa-desktop" )
        , ( "userAgent.mobile", "fas fa-mobile-alt" )
        , ( "userAgent.tdk", "fas fa-terminal" )
        ]
