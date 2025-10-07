module Common.Components.FontAwesome exposing
    ( fa
    , faActiveSessionRevoke
    , faAdd
    , faArrowRight
    , faArrowRightMS
    , faCancel
    , faChangeLanguage
    , faClose
    , faCopy
    , faDelete
    , faDetailShowAll
    , faDisable
    , faDocumentTemplateEditorFiles
    , faDocumentTemplateEditorPublish
    , faDocumentTemplateRestore
    , faDocumentTemplateSetDeprecated
    , faDocumentsDownload
    , faDocumentsSubmit
    , faDocumentsViewError
    , faDownload
    , faEdit
    , faEnable
    , faError
    , faExport
    , faExternalLink
    , faFw
    , faGuideLink
    , faImportFile
    , faInfo
    , faKmAnswer
    , faKmChapter
    , faKmChoice
    , faKmDetailCreateKmEditor
    , faKmDetailCreateQuestionnaire
    , faKmDetailFork
    , faKmDetailRegistryLink
    , faKmEditorCollapseAll
    , faKmEditorCopyUuid
    , faKmEditorExpandAll
    , faKmEditorKnowledgeModel
    , faKmEditorListContinueMigration
    , faKmEditorListEdit
    , faKmEditorListEdited
    , faKmEditorListPublish
    , faKmEditorListUpdate
    , faKmEditorMove
    , faKmEditorTags
    , faKmEditorTreeClosed
    , faKmEditorTreeOpened
    , faKmExpert
    , faKmFork
    , faKmImportFromFile
    , faKmImportFromOwl
    , faKmImportFromRegistry
    , faKmIntegration
    , faKmItemTemplate
    , faKmKnowledgeModel
    , faKmMetric
    , faKmPhase
    , faKmQuestion
    , faKmReference
    , faKmResourceCollection
    , faKmResourcePage
    , faKmTag
    , faKmsUpload
    , faKnowledgeModel
    , faListingActions
    , faListingFilterMultiNotSelected
    , faListingFilterMultiSelected
    , faListingFilterSingleNotSelected
    , faListingFilterSingleSelected
    , faLocaleCreate
    , faLocaleDefault
    , faLocaleImport
    , faLoginExternalService
    , faMenuAbout
    , faMenuAdministration
    , faMenuAssignedComments
    , faMenuCollapse
    , faMenuDashboard
    , faMenuDev
    , faMenuKnowledgeModels
    , faMenuLogout
    , faMenuOpen
    , faMenuProfile
    , faMenuProjects
    , faMenuReportIssue
    , faMenuTemplates
    , faMenuTenants
    , faNext
    , faOpen
    , faPersistentCommandRetry
    , faPrev
    , faPreview
    , faPrivacyPolicy
    , faProjectDocuments
    , faProjectFiles
    , faProjectMetrics
    , faProjectQuestionnaire
    , faProjectSharingInternal
    , faProjectSharingPrivate
    , faProjectSharingPublic
    , faQuestionnaire
    , faQuestionnaireAnsweredIndication
    , faQuestionnaireClearAnswer
    , faQuestionnaireComments
    , faQuestionnaireCommentsAssign
    , faQuestionnaireCommentsResolve
    , faQuestionnaireCopyLink
    , faQuestionnaireCopyLinkCopied
    , faQuestionnaireDesirable
    , faQuestionnaireExpand
    , faQuestionnaireExperts
    , faQuestionnaireFeedback
    , faQuestionnaireFollowUpsIndication
    , faQuestionnaireHistoryCreateDocument
    , faQuestionnaireHistoryRevert
    , faQuestionnaireItemCollapse
    , faQuestionnaireItemCollapseAll
    , faQuestionnaireItemExpand
    , faQuestionnaireItemExpandAll
    , faQuestionnaireItemMoveDown
    , faQuestionnaireItemMoveUp
    , faQuestionnaireListClone
    , faQuestionnaireListCreateMigration
    , faQuestionnaireListCreateProjectFromTemplate
    , faQuestionnaireMigrationResolve
    , faQuestionnaireMigrationResolveAll
    , faQuestionnaireMigrationUndo
    , faQuestionnaireResourcePageReferences
    , faQuestionnaireSavingSaved
    , faQuestionnaireSavingSaving
    , faQuestionnaireSettingsKmAllQuestions
    , faQuestionnaireSettingsKmFiltered
    , faQuestionnaireShrink
    , faQuestionnaireUrlReferences
    , faRemove
    , faRemoveFw
    , faSearch
    , faSecretHide
    , faSecretShow
    , faSettings
    , faSortAsc
    , faSortDesc
    , faSpinner
    , faSuccess
    , faTermsOfService
    , faUserAgentDesktop
    , faUserAgentMobile
    , faUserAgentTdk
    , faView
    , faWarning
    , fab
    , far
    , farFw
    , fas
    , fasFw
    )

import Html exposing (Html, i)
import Html.Attributes exposing (class)


fa : String -> Html msg
fa icon =
    i [ class ("fa " ++ icon) ] []


faFw : String -> Html msg
faFw icon =
    i [ class ("fa fa-fw " ++ icon) ] []


fas : String -> Html msg
fas icon =
    i [ class ("fas " ++ icon) ] []


fasFw : String -> Html msg
fasFw icon =
    i [ class ("fas fa-fw " ++ icon) ] []


far : String -> Html msg
far icon =
    i [ class ("far " ++ icon) ] []


farFw : String -> Html msg
farFw icon =
    i [ class ("far fa-fw " ++ icon) ] []


fab : String -> Html msg
fab icon =
    i [ class ("fab " ++ icon) ] []



-- Specific icons


faActiveSessionRevoke : Html msg
faActiveSessionRevoke =
    fas "fa-ban"


faAdd : Html msg
faAdd =
    fas "fa-plus"


faArrowRight : Html msg
faArrowRight =
    fas "fa-long-arrow-alt-right"


faArrowRightMS : Html msg
faArrowRightMS =
    fas "fa-arrow-right-long ms-2"


faCancel : Html msg
faCancel =
    fas "fa-ban"


faChangeLanguage : Html msg
faChangeLanguage =
    fas "fa-language fa=fw"


faClose : Html msg
faClose =
    fas "fa-times"


faCopy : Html msg
faCopy =
    fas "fa-paste"


faDelete : Html msg
faDelete =
    fas "fa-trash"


faDetailShowAll : Html msg
faDetailShowAll =
    fas "fa-angle-down"


faDisable : Html msg
faDisable =
    fas "fa-toggle-off"


faDocumentTemplateEditorFiles : Html msg
faDocumentTemplateEditorFiles =
    far "fa-file-code"


faDocumentTemplateEditorPublish : Html msg
faDocumentTemplateEditorPublish =
    fas "fa-cloud-upload-alt"


faDocumentTemplateRestore : Html msg
faDocumentTemplateRestore =
    fas "fa-undo-alt"


faDocumentTemplateSetDeprecated : Html msg
faDocumentTemplateSetDeprecated =
    fas "fa-ban"


faDocumentsDownload : Html msg
faDocumentsDownload =
    fas "fa-download"


faDocumentsSubmit : Html msg
faDocumentsSubmit =
    fas "fa-paper-plane"


faDocumentsViewError : Html msg
faDocumentsViewError =
    fas "fa-exclamation-circle"


faDownload : Html msg
faDownload =
    fas "fa-download"


faEdit : Html msg
faEdit =
    fas "fa-edit"


faEnable : Html msg
faEnable =
    fas "fa-toggle-on"


faError : Html msg
faError =
    fas "fa-exclamation-circle"


faExport : Html msg
faExport =
    fas "fa-download"


faExternalLink : Html msg
faExternalLink =
    fas "fa-external-link-alt"


faGuideLink : Html msg
faGuideLink =
    fas "fa-circle-question"


faImportFile : Html msg
faImportFile =
    far "fa-file"


faInfo : Html msg
faInfo =
    fas "fa-info-circle"


faKmAnswer : Html msg
faKmAnswer =
    far "fa-dot-circle"


faKmChapter : Html msg
faKmChapter =
    far "fa-file"


faKmChoice : Html msg
faKmChoice =
    far "fa-check-square"


faKmDetailCreateKmEditor : Html msg
faKmDetailCreateKmEditor =
    fas "fa-edit"


faKmDetailCreateQuestionnaire : Html msg
faKmDetailCreateQuestionnaire =
    far "fa-list-alt"


faKmDetailFork : Html msg
faKmDetailFork =
    fas "fa-code-branch"


faKmDetailRegistryLink : Html msg
faKmDetailRegistryLink =
    fas "fa-external-link-alt"


faKmEditorCollapseAll : Html msg
faKmEditorCollapseAll =
    fas "fa-angle-double-up"


faKmEditorCopyUuid : Html msg
faKmEditorCopyUuid =
    fas "fa-paste"


faKmEditorExpandAll : Html msg
faKmEditorExpandAll =
    fas "fa-angle-double-down"


faKmEditorKnowledgeModel : Html msg
faKmEditorKnowledgeModel =
    fas "fa-sitemap"


faKmEditorListContinueMigration : Html msg
faKmEditorListContinueMigration =
    fas "fa-long-arrow-alt-right"


faKmEditorListEdit : Html msg
faKmEditorListEdit =
    fas "fa-pen"


faKmEditorListEdited : Html msg
faKmEditorListEdited =
    fas "fa-pen"


faKmEditorListPublish : Html msg
faKmEditorListPublish =
    fas "fa-cloud-upload-alt"


faKmEditorListUpdate : Html msg
faKmEditorListUpdate =
    fas "fa-sort-amount-up"


faKmEditorMove : Html msg
faKmEditorMove =
    fas "fa-reply"


faKmEditorTags : Html msg
faKmEditorTags =
    fas "fa-tags"


faKmEditorTreeClosed : Html msg
faKmEditorTreeClosed =
    fas "fa-caret-right"


faKmEditorTreeOpened : Html msg
faKmEditorTreeOpened =
    fas "fa-caret-down"


faKmExpert : Html msg
faKmExpert =
    far "fa-user"


faKmFork : Html msg
faKmFork =
    fas "fa-code-branch"


faKmImportFromFile : Html msg
faKmImportFromFile =
    fas "fa-upload"


faKmImportFromOwl : Html msg
faKmImportFromOwl =
    fas "fa-project-diagram"


faKmImportFromRegistry : Html msg
faKmImportFromRegistry =
    fas "fa-cloud-download-alt"


faKmIntegration : Html msg
faKmIntegration =
    fas "fa-exchange-alt"


faKmItemTemplate : Html msg
faKmItemTemplate =
    far "fa-file-alt"


faKmKnowledgeModel : Html msg
faKmKnowledgeModel =
    fas "fa-database"


faKmMetric : Html msg
faKmMetric =
    fas "fa-chart-column"


faKmPhase : Html msg
faKmPhase =
    far "fa-clock"


faKmQuestion : Html msg
faKmQuestion =
    far "fa-comment"


faKmReference : Html msg
faKmReference =
    far "fa-bookmark"


faKmResourceCollection : Html msg
faKmResourceCollection =
    fas "fa-book"


faKmResourcePage : Html msg
faKmResourcePage =
    far "fa-file-lines"


faKmTag : Html msg
faKmTag =
    fas "fa-tag"


faKmsUpload : Html msg
faKmsUpload =
    fas "fa-upload"


faKnowledgeModel : Html msg
faKnowledgeModel =
    fas "fa-sitemap"


faListingActions : Html msg
faListingActions =
    fas "fa-ellipsis-v"


faListingFilterMultiNotSelected : Html msg
faListingFilterMultiNotSelected =
    far "fa-square"


faListingFilterMultiSelected : Html msg
faListingFilterMultiSelected =
    fas "fa-check-square"


faListingFilterSingleNotSelected : Html msg
faListingFilterSingleNotSelected =
    far "fa-circle"


faListingFilterSingleSelected : Html msg
faListingFilterSingleSelected =
    fas "fa-check-circle"


faLocaleCreate : Html msg
faLocaleCreate =
    fas "fa-plus"


faLocaleDefault : Html msg
faLocaleDefault =
    fas "fa-check-circle"


faLocaleImport : Html msg
faLocaleImport =
    fas "fa-upload"


faLoginExternalService : Html msg
faLoginExternalService =
    fab "fa-openid"


faMenuAbout : Html msg
faMenuAbout =
    fas "fa-info fa-fw"


faMenuAdministration : Html msg
faMenuAdministration =
    fas "fa-cog fa-fw"


faMenuAssignedComments : Html msg
faMenuAssignedComments =
    fas "fa-check-to-slot fa-fw"


faMenuCollapse : Html msg
faMenuCollapse =
    fas "fa-angle-double-left fa-fw"


faMenuDashboard : Html msg
faMenuDashboard =
    fas "fa-home fa-fw"


faMenuDev : Html msg
faMenuDev =
    fas "fa-laptop-code fa-fw"


faMenuKnowledgeModels : Html msg
faMenuKnowledgeModels =
    fas "fa-sitemap fa-fw"


faMenuLogout : Html msg
faMenuLogout =
    fas "fa-sign-out-alt fa-fw"


faMenuOpen : Html msg
faMenuOpen =
    fas "fa-angle-double-right fa-fw"


faMenuProfile : Html msg
faMenuProfile =
    fas "fa-user-edit fa-fw"


faMenuProjects : Html msg
faMenuProjects =
    fas "fa-folder fa-fw"


faMenuReportIssue : Html msg
faMenuReportIssue =
    fas "fa-exclamation-triangle fa-fw"


faMenuTemplates : Html msg
faMenuTemplates =
    fas "fa-file-code fa-fw"


faMenuTenants : Html msg
faMenuTenants =
    fas "fa-server fa-fw"


faNext : Html msg
faNext =
    fas "fa-chevron-right"


faOpen : Html msg
faOpen =
    far "fa-folder-open"


faPersistentCommandRetry : Html msg
faPersistentCommandRetry =
    fas "fa-sync-alt"


faPrev : Html msg
faPrev =
    fas "fa-chevron-left"


faPreview : Html msg
faPreview =
    fas "fa-eye"


faPrivacyPolicy : Html msg
faPrivacyPolicy =
    fas "fa-file-shield"


faProjectDocuments : Html msg
faProjectDocuments =
    far "fa-copy"


faProjectFiles : Html msg
faProjectFiles =
    far "fa-folder-closed"


faProjectMetrics : Html msg
faProjectMetrics =
    far "fa-chart-bar"


faProjectQuestionnaire : Html msg
faProjectQuestionnaire =
    far "fa-list-alt"


faProjectSharingInternal : Html msg
faProjectSharingInternal =
    fas "fa-user-friends"


faProjectSharingPrivate : Html msg
faProjectSharingPrivate =
    fas "fa-lock"


faProjectSharingPublic : Html msg
faProjectSharingPublic =
    fas "fa-globe"


faQuestionnaire : Html msg
faQuestionnaire =
    far "fa-list-alt"


faQuestionnaireAnsweredIndication : Html msg
faQuestionnaireAnsweredIndication =
    fas "fa-check"


faQuestionnaireClearAnswer : Html msg
faQuestionnaireClearAnswer =
    fas "fa-undo-alt"


faQuestionnaireComments : Html msg
faQuestionnaireComments =
    fas "fa-comments"


faQuestionnaireCommentsAssign : Html msg
faQuestionnaireCommentsAssign =
    fas "fa-user-plus"


faQuestionnaireCommentsResolve : Html msg
faQuestionnaireCommentsResolve =
    fas "fa-check"


faQuestionnaireCopyLink : Html msg
faQuestionnaireCopyLink =
    fas "fa-link"


faQuestionnaireCopyLinkCopied : Html msg
faQuestionnaireCopyLinkCopied =
    fas "fa-check"


faQuestionnaireDesirable : Html msg
faQuestionnaireDesirable =
    far "fa-check-square"


faQuestionnaireExpand : Html msg
faQuestionnaireExpand =
    fas "fa-expand"


faQuestionnaireExperts : Html msg
faQuestionnaireExperts =
    far "fa-address-book"


faQuestionnaireFeedback : Html msg
faQuestionnaireFeedback =
    fas "fa-exclamation"


faQuestionnaireFollowUpsIndication : Html msg
faQuestionnaireFollowUpsIndication =
    fas "fa-list-ul"


faQuestionnaireHistoryCreateDocument : Html msg
faQuestionnaireHistoryCreateDocument =
    far "fa-file"


faQuestionnaireHistoryRevert : Html msg
faQuestionnaireHistoryRevert =
    fas "fa-history"


faQuestionnaireItemCollapse : Html msg
faQuestionnaireItemCollapse =
    fas "fa-chevron-up"


faQuestionnaireItemCollapseAll : Html msg
faQuestionnaireItemCollapseAll =
    fas "fa-angle-double-up"


faQuestionnaireItemExpand : Html msg
faQuestionnaireItemExpand =
    fas "fa-chevron-down"


faQuestionnaireItemExpandAll : Html msg
faQuestionnaireItemExpandAll =
    fas "fa-angle-double-down"


faQuestionnaireItemMoveDown : Html msg
faQuestionnaireItemMoveDown =
    fas "fa-arrow-down"


faQuestionnaireItemMoveUp : Html msg
faQuestionnaireItemMoveUp =
    fas "fa-arrow-up"


faQuestionnaireListClone : Html msg
faQuestionnaireListClone =
    far "fa-copy"


faQuestionnaireListCreateMigration : Html msg
faQuestionnaireListCreateMigration =
    fas "fa-random"


faQuestionnaireListCreateProjectFromTemplate : Html msg
faQuestionnaireListCreateProjectFromTemplate =
    far "fa-list-alt"


faQuestionnaireMigrationResolve : Html msg
faQuestionnaireMigrationResolve =
    fas "fa-check"


faQuestionnaireMigrationResolveAll : Html msg
faQuestionnaireMigrationResolveAll =
    fas "fa-check-double"


faQuestionnaireMigrationUndo : Html msg
faQuestionnaireMigrationUndo =
    fas "fa-undo-alt"


faQuestionnaireResourcePageReferences : Html msg
faQuestionnaireResourcePageReferences =
    fas "fa-book"


faQuestionnaireSavingSaved : Html msg
faQuestionnaireSavingSaved =
    far "fa-check-circle"


faQuestionnaireSavingSaving : Html msg
faQuestionnaireSavingSaving =
    fas "fa-sync-alt fa-spin"


faQuestionnaireSettingsKmAllQuestions : Html msg
faQuestionnaireSettingsKmAllQuestions =
    far "fa-check-square"


faQuestionnaireSettingsKmFiltered : Html msg
faQuestionnaireSettingsKmFiltered =
    fas "fa-filter"


faQuestionnaireShrink : Html msg
faQuestionnaireShrink =
    fas "fa-compress"


faQuestionnaireUrlReferences : Html msg
faQuestionnaireUrlReferences =
    fas "fa-external-link-alt"


faRemove : Html msg
faRemove =
    fas "fa-times"


faRemoveFw : Html msg
faRemoveFw =
    fas "fa-fw fa-times"


faSearch : Html msg
faSearch =
    fas "fa-magnifying-glass"


faSecretHide : Html msg
faSecretHide =
    fas "fa-eye-slash"


faSecretShow : Html msg
faSecretShow =
    fas "fa-eye"


faSettings : Html msg
faSettings =
    fas "fa-cogs"


faSortAsc : Html msg
faSortAsc =
    fas "fa-arrow-down-long"


faSortDesc : Html msg
faSortDesc =
    fas "fa-arrow-up-long"


faSpinner : Html msg
faSpinner =
    fas "fa-spinner fa-spin"


faSuccess : Html msg
faSuccess =
    fas "fa-check"


faTermsOfService : Html msg
faTermsOfService =
    fas "fa-file-signature"


faUserAgentDesktop : Html msg
faUserAgentDesktop =
    fas "fa-desktop fa-fw"


faUserAgentMobile : Html msg
faUserAgentMobile =
    fas "fa-mobile-alt fa-fw"


faUserAgentTdk : Html msg
faUserAgentTdk =
    fas "fa-terminal fa-fw"


faView : Html msg
faView =
    far "fa-eye"


faWarning : Html msg
faWarning =
    fas "fa-exclamation-triangle"
