module Wizard.Common.Provisioning.DefaultIconSet exposing (iconSet)

import Dict exposing (Dict)


iconSet : Dict String String
iconSet =
    Dict.fromList
        [ ( "_global.add", "fas fa-plus" )
        , ( "_global.arrowLeft", "fas fa-long-arrow-alt-left" )
        , ( "_global.arrowRight", "fas fa-long-arrow-alt-right" )
        , ( "_global.cancel", "fas fa-ban" )
        , ( "_global.delete", "fas fa-trash" )
        , ( "_global.edit", "fas fa-edit" )
        , ( "_global.error", "fas fa-exclamation-circle" )
        , ( "_global.info", "fas fa-info-circle" )
        , ( "_global.remove", "fas fa-times" )
        , ( "_global.spinner", "fas fa-spinner fa-spin" )
        , ( "_global.success", "fas fa-check" )
        , ( "_global.warning", "fas fa-exclamation-triangle" )
        , ( "colorButton.check", "fas fa-check" )
        , ( "format.code", "far fa-file-code" )
        , ( "format.pdf", "far fa-file-pdf" )
        , ( "format.text", "far fa-file-alt" )
        , ( "format.word", "far fa-file-word" )
        , ( "km.answer", "far fa-check-square" )
        , ( "km.chapter", "far fa-file" )
        , ( "km.expert", "far fa-user" )
        , ( "km.fork", "fas fa-code-branch" )
        , ( "km.integration", "fas fa-exchange-alt" )
        , ( "km.itemTemplate", "far fa-file-alt" )
        , ( "km.knowledgeModel", "fas fa-database" )
        , ( "km.question", "far fa-comment" )
        , ( "km.reference", "far fa-bookmark" )
        , ( "km.tag", "fas fa-tag" )
        , ( "kmEditor.copyUuid", "fas fa-paste" )
        , ( "kmEditor.knowledgeModel", "fas fa-sitemap" )
        , ( "kmEditor.move", "fas fa-reply" )
        , ( "kmEditor.preview", "fas fa-eye" )
        , ( "kmEditor.tags", "fas fa-tags" )
        , ( "kmEditor.treeOpened", "fas fa-caret-down" )
        , ( "kmEditor.treeClosed", "fas fa-caret-right" )
        , ( "kmEditorList.continueMigration", "fas fa-long-arrow-alt-right" )
        , ( "kmEditorList.edited", "fas fa-pen" )
        , ( "kmEditorList.publish", "fas fa-cloud-upload-alt" )
        , ( "kmEditorList.upgrade", "fas fa-sort-amount-up" )
        , ( "kmDetail.createKMEditor", "fas fa-edit" )
        , ( "kmDetail.createQuestionnaire", "far fa-list-alt" )
        , ( "kmDetail.export", "fas fa-download" )
        , ( "kmDetail.registryLink", "fas fa-external-link-alt" )
        , ( "kmImport.file", "fas fa-file" )
        , ( "kmImport.fromFile", "fas fa-upload" )
        , ( "kmImport.fromRegistry", "fas fa-cloud-download-alt" )
        , ( "kms.upload", "fas fa-upload" )
        , ( "menu.about", "fas fa-info" )
        , ( "menu.collapse", "fas fa-angle-double-left" )
        , ( "menu.dropdownToggle", "fas fa-angle-right" )
        , ( "menu.help", "fas fa-question-circle" )
        , ( "menu.kmEditor", "fas fa-edit" )
        , ( "menu.knowledgeModels", "fas fa-sitemap" )
        , ( "menu.logout", "fas fa-sign-out-alt" )
        , ( "menu.open", "fas fa-angle-double-right" )
        , ( "menu.organization", "fas fa-building" )
        , ( "menu.profile", "fas fa-user" )
        , ( "menu.questionnaires", "fas fa-list-alt" )
        , ( "menu.reportIssue", "fas fa-exclamation-triangle" )
        , ( "menu.user", "fas fa-user-circle" )
        , ( "menu.users", "fas fa-users" )
        , ( "questionnaire.answeredIndication", "fas fa-check" )
        , ( "questionnaire.clearAnswer", "fas fa-undo-alt" )
        , ( "questionnaire.desirable", "far fa-check-square" )
        , ( "questionnaire.experts", "far fa-address-book" )
        , ( "questionnaire.feedback", "fas fa-exclamation" )
        , ( "questionnaire.followUpsIndication", "fas fa-list-ul" )
        , ( "questionnaire.resourcePageReferences", "fas fa-book" )
        , ( "questionnaire.urlReferences", "fas fa-external-link-alt" )
        , ( "questionnaireList.createMigration", "fas fa-random" )
        , ( "questionnaireList.export", "fas fa-download" )
        , ( "questionnaireList.owner", "fas fa-user" )
        , ( "questionnaireMigration.resolve", "fas fa-check" )
        , ( "questionnaireMigration.undo", "fas fa-undo-alt" )
        , ( "userCard.icon", "far fa-user-circle" )
        ]
