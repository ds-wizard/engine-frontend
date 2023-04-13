module Wizard.Common.Feature exposing
    ( LocaleLike
    , apps
    , dev
    , documentDelete
    , documentDownload
    , documentSubmit
    , documentTemplatesDelete
    , documentTemplatesExport
    , documentTemplatesImport
    , documentTemplatesView
    , documentsView
    , isDefaultLanguage
    , knowledgeModelEditorCancelMigration
    , knowledgeModelEditorContinueMigration
    , knowledgeModelEditorDelete
    , knowledgeModelEditorOpen
    , knowledgeModelEditorPublish
    , knowledgeModelEditorUpgrade
    , knowledgeModelEditorsCreate
    , knowledgeModelEditorsEdit
    , knowledgeModelEditorsPublish
    , knowledgeModelEditorsUpgrade
    , knowledgeModelEditorsView
    , knowledgeModelsDelete
    , knowledgeModelsExport
    , knowledgeModelsImport
    , knowledgeModelsPreview
    , knowledgeModelsView
    , localeChangeEnabled
    , localeCreate
    , localeDelete
    , localeExport
    , localeImport
    , localeSetDefault
    , localeView
    , projectCancelMigration
    , projectClone
    , projectCommentAdd
    , projectCommentDelete
    , projectCommentEdit
    , projectCommentPrivate
    , projectCommentThreadDelete
    , projectCommentThreadReopen
    , projectCommentThreadResolve
    , projectContinueMigration
    , projectCreateMigration
    , projectDelete
    , projectDocumentsView
    , projectImporters
    , projectMetrics
    , projectOpen
    , projectPreview
    , projectSettings
    , projectTagging
    , projectTemplatesCreate
    , projectTodos
    , projectVersionHitory
    , projectsCreateCustom
    , projectsCreateFromTemplate
    , projectsView
    , settings
    , userEdit
    , userEditActiveSessions
    , userEditApiKeys
    , usersCreate
    , usersView
    )

import Shared.Auth.Permission as Perm
import Shared.Common.UuidOrCurrent as UuidOrCurrent exposing (UuidOrCurrent)
import Shared.Data.Branch as Branch exposing (Branch)
import Shared.Data.Branch.BranchState as BranchState
import Shared.Data.Document as Document exposing (Document)
import Shared.Data.Document.DocumentState exposing (DocumentState(..))
import Shared.Data.Questionnaire as Questionnaire exposing (Questionnaire)
import Shared.Data.Questionnaire.QuestionnaireCreation as QuestionnaireCreation
import Shared.Data.Questionnaire.QuestionnaireState as QuestionnaireState
import Shared.Data.QuestionnaireDetail as QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Data.QuestionnaireDetail.Comment as Comment exposing (Comment)
import Shared.Data.QuestionnaireDetail.CommentThread as CommentThread exposing (CommentThread)
import Shared.Data.UserInfo as UserInfo
import Wizard.Common.AppState exposing (AppState)



-- Knowledge Model Editors


knowledgeModelEditorsView : AppState -> Bool
knowledgeModelEditorsView =
    adminOr Perm.knowledgeModel


knowledgeModelEditorsCreate : AppState -> Bool
knowledgeModelEditorsCreate =
    adminOr Perm.knowledgeModel


knowledgeModelEditorsEdit : AppState -> Bool
knowledgeModelEditorsEdit =
    adminOr Perm.knowledgeModel


knowledgeModelEditorsUpgrade : AppState -> Bool
knowledgeModelEditorsUpgrade =
    adminOr Perm.knowledgeModelUpgrade


knowledgeModelEditorsPublish : AppState -> Bool
knowledgeModelEditorsPublish =
    adminOr Perm.knowledgeModelPublish


knowledgeModelEditorOpen : AppState -> Branch -> Bool
knowledgeModelEditorOpen appState branch =
    adminOr Perm.knowledgeModel appState
        && Branch.matchState [ BranchState.Default, BranchState.Edited, BranchState.Outdated ] branch


knowledgeModelEditorPublish : AppState -> Branch -> Bool
knowledgeModelEditorPublish appState branch =
    adminOr Perm.knowledgeModelPublish appState
        && Branch.matchState [ BranchState.Edited, BranchState.Migrated ] branch


knowledgeModelEditorUpgrade : AppState -> Branch -> Bool
knowledgeModelEditorUpgrade appState branch =
    adminOr Perm.knowledgeModelUpgrade appState
        && Branch.matchState [ BranchState.Outdated ] branch


knowledgeModelEditorContinueMigration : AppState -> Branch -> Bool
knowledgeModelEditorContinueMigration appState branch =
    adminOr Perm.knowledgeModelUpgrade appState
        && Branch.matchState [ BranchState.Migrating ] branch


knowledgeModelEditorCancelMigration : AppState -> Branch -> Bool
knowledgeModelEditorCancelMigration appState branch =
    adminOr Perm.knowledgeModelUpgrade appState
        && Branch.matchState [ BranchState.Migrating, BranchState.Migrated ] branch


knowledgeModelEditorDelete : AppState -> Branch -> Bool
knowledgeModelEditorDelete appState _ =
    adminOr Perm.knowledgeModel appState



-- Knowledge Models


knowledgeModelsView : AppState -> Bool
knowledgeModelsView =
    adminOr Perm.packageManagementRead


knowledgeModelsImport : AppState -> Bool
knowledgeModelsImport =
    adminOr Perm.packageManagementWrite


knowledgeModelsExport : AppState -> Bool
knowledgeModelsExport =
    adminOr Perm.packageManagementWrite


knowledgeModelsDelete : AppState -> Bool
knowledgeModelsDelete =
    adminOr Perm.packageManagementWrite


knowledgeModelsPreview : AppState -> Bool
knowledgeModelsPreview _ =
    True



-- Document Templates


documentTemplatesView : AppState -> Bool
documentTemplatesView =
    adminOr Perm.documentTemplates


documentTemplatesImport : AppState -> Bool
documentTemplatesImport =
    adminOr Perm.packageManagementWrite


documentTemplatesExport : AppState -> Bool
documentTemplatesExport =
    adminOr Perm.documentTemplates


documentTemplatesDelete : AppState -> Bool
documentTemplatesDelete =
    adminOr Perm.packageManagementWrite



-- Projects


projectsView : AppState -> Bool
projectsView =
    adminOr Perm.questionnaire


projectsCreateCustom : AppState -> Bool
projectsCreateCustom appState =
    let
        canCreateCustomProjects =
            QuestionnaireCreation.customEnabled appState.config.questionnaire.questionnaireCreation

        canCreateProjectTemplates =
            adminOr Perm.questionnaireTemplate appState

        canCreateAnonymousProjects =
            appState.config.questionnaire.questionnaireSharing.anonymousEnabled
    in
    (canCreateAnonymousProjects || adminOr Perm.questionnaire appState) && (canCreateCustomProjects || canCreateProjectTemplates)


projectsCreateFromTemplate : AppState -> Bool
projectsCreateFromTemplate appState =
    let
        canCreateFromTemplates =
            QuestionnaireCreation.fromTemplateEnabled appState.config.questionnaire.questionnaireCreation
    in
    adminOr Perm.questionnaire appState && canCreateFromTemplates


projectTemplatesCreate : AppState -> Bool
projectTemplatesCreate =
    adminOr Perm.questionnaireTemplate


projectOpen : AppState -> Questionnaire -> Bool
projectOpen _ questionnaire =
    questionnaire.state /= QuestionnaireState.Migrating


projectClone : AppState -> Questionnaire -> Bool
projectClone _ questionnaire =
    questionnaire.state /= QuestionnaireState.Migrating


projectCreateMigration : AppState -> Questionnaire -> Bool
projectCreateMigration appState questionnaire =
    Questionnaire.isEditable appState questionnaire && questionnaire.state /= QuestionnaireState.Migrating


projectContinueMigration : AppState -> Questionnaire -> Bool
projectContinueMigration appState questionnaire =
    Questionnaire.isEditable appState questionnaire && questionnaire.state == QuestionnaireState.Migrating


projectCancelMigration : AppState -> Questionnaire -> Bool
projectCancelMigration appState questionnaire =
    Questionnaire.isEditable appState questionnaire && questionnaire.state == QuestionnaireState.Migrating


projectDelete : AppState -> Questionnaire -> Bool
projectDelete appState questionnaire =
    Questionnaire.isEditable appState questionnaire


projectTagging : AppState -> Bool
projectTagging appState =
    appState.config.questionnaire.projectTagging.enabled


projectMetrics : AppState -> QuestionnaireDetail -> Bool
projectMetrics appState _ =
    appState.config.questionnaire.summaryReport.enabled


projectPreview : AppState -> QuestionnaireDetail -> Bool
projectPreview _ _ =
    True


projectDocumentsView : AppState -> QuestionnaireDetail -> Bool
projectDocumentsView _ _ =
    True


projectTodos : AppState -> QuestionnaireDetail -> Bool
projectTodos appState questionnaire =
    QuestionnaireDetail.isEditor appState questionnaire && not (QuestionnaireDetail.isMigrating questionnaire)


projectVersionHitory : AppState -> QuestionnaireDetail -> Bool
projectVersionHitory appState questionnaire =
    QuestionnaireDetail.isEditor appState questionnaire && not (QuestionnaireDetail.isMigrating questionnaire)


projectSettings : AppState -> QuestionnaireDetail -> Bool
projectSettings appState questionnaire =
    QuestionnaireDetail.isOwner appState questionnaire


projectCommentAdd : AppState -> QuestionnaireDetail -> Bool
projectCommentAdd appState questionnaire =
    QuestionnaireDetail.canComment appState questionnaire


projectCommentEdit : AppState -> QuestionnaireDetail -> CommentThread -> Comment -> Bool
projectCommentEdit appState questionnaire commentThread comment =
    QuestionnaireDetail.canComment appState questionnaire && not commentThread.resolved && Comment.isAuthor appState.session.user comment


projectCommentDelete : AppState -> QuestionnaireDetail -> CommentThread -> Comment -> Bool
projectCommentDelete appState questionnaire commentThread comment =
    QuestionnaireDetail.canComment appState questionnaire && not commentThread.resolved && Comment.isAuthor appState.session.user comment


projectCommentThreadResolve : AppState -> QuestionnaireDetail -> CommentThread -> Bool
projectCommentThreadResolve appState questionnaire commentThread =
    QuestionnaireDetail.canComment appState questionnaire && not commentThread.resolved


projectCommentThreadReopen : AppState -> QuestionnaireDetail -> CommentThread -> Bool
projectCommentThreadReopen appState questionnaire commentThread =
    QuestionnaireDetail.canComment appState questionnaire && commentThread.resolved


projectCommentThreadDelete : AppState -> QuestionnaireDetail -> CommentThread -> Bool
projectCommentThreadDelete appState questionnaire commentThread =
    QuestionnaireDetail.canComment appState questionnaire && CommentThread.isAuthor appState.session.user commentThread


projectCommentPrivate : AppState -> QuestionnaireDetail -> Bool
projectCommentPrivate appState questionnaire =
    QuestionnaireDetail.isEditor appState questionnaire



-- Project Importers


projectImporters : AppState -> Bool
projectImporters =
    adminOr Perm.questionnaireImporter



-- Documents


documentsView : AppState -> Bool
documentsView =
    isAdmin


documentDelete : AppState -> Document -> Bool
documentDelete appState document =
    isAdmin appState || Document.isOwner appState document


documentDownload : AppState -> Document -> Bool
documentDownload _ document =
    document.state == DoneDocumentState


documentSubmit : AppState -> Document -> Bool
documentSubmit appState document =
    (document.state == DoneDocumentState)
        && appState.config.submission.enabled
        && adminOr Perm.submission appState



-- Settings


settings : AppState -> Bool
settings =
    adminOr Perm.settings



-- Users


usersView : AppState -> Bool
usersView =
    adminOr Perm.userManagement


usersCreate : AppState -> Bool
usersCreate =
    adminOr Perm.userManagement


userEdit : AppState -> UuidOrCurrent -> Bool
userEdit appState uuidOrCurrent =
    UuidOrCurrent.isCurrent uuidOrCurrent || adminOr Perm.userManagement appState


userEditApiKeys : UuidOrCurrent -> Bool
userEditApiKeys =
    UuidOrCurrent.isCurrent


userEditActiveSessions : UuidOrCurrent -> Bool
userEditActiveSessions =
    UuidOrCurrent.isCurrent



-- Locale


type alias LocaleLike a =
    { a
        | localeId : String
        , organizationId : String
        , defaultLocale : Bool
        , enabled : Bool
    }


isDefaultLanguage : LocaleLike a -> Bool
isDefaultLanguage locale =
    locale.organizationId == "wizard" && locale.localeId == "default"


localeView : AppState -> Bool
localeView =
    adminOr Perm.locale


localeCreate : AppState -> Bool
localeCreate =
    adminOr Perm.locale


localeImport : AppState -> Bool
localeImport =
    adminOr Perm.locale


localeExport : AppState -> LocaleLike a -> Bool
localeExport appState locale =
    adminOr Perm.locale appState
        && not (isDefaultLanguage locale)


localeSetDefault : AppState -> LocaleLike a -> Bool
localeSetDefault appState locale =
    adminOr Perm.locale appState
        && locale.enabled
        && not locale.defaultLocale


localeChangeEnabled : AppState -> LocaleLike a -> Bool
localeChangeEnabled appState locale =
    adminOr Perm.locale appState
        && not locale.defaultLocale


localeDelete : AppState -> LocaleLike a -> Bool
localeDelete appState locale =
    adminOr Perm.locale appState
        && not (isDefaultLanguage locale)
        && not locale.defaultLocale



-- Apps


apps : AppState -> Bool
apps appState =
    Perm.hasPerm appState.session Perm.apps



-- Dev


dev : AppState -> Bool
dev appState =
    Perm.hasPerm appState.session Perm.dev



-- Helpers


isAdmin : AppState -> Bool
isAdmin appState =
    UserInfo.isAdmin appState.session.user


adminOr : String -> AppState -> Bool
adminOr perm appState =
    isAdmin appState || Perm.hasPerm appState.session perm
