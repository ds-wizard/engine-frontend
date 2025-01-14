module Wizard.Common.Feature exposing
    ( LocaleLike
    , dev
    , documentDelete
    , documentDownload
    , documentSubmit
    , documentTemplatesDelete
    , documentTemplatesExport
    , documentTemplatesImport
    , documentTemplatesView
    , documentsView
    , isAdmin
    , isDataSteward
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
    , knowledgeModelRestore
    , knowledgeModelSetDeprecated
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
    , projectActions
    , projectCancelMigration
    , projectClone
    , projectCommentAdd
    , projectCommentDelete
    , projectCommentEdit
    , projectCommentPrivate
    , projectCommentThreadAssign
    , projectCommentThreadDelete
    , projectCommentThreadRemoveAssign
    , projectCommentThreadReopen
    , projectCommentThreadResolve
    , projectContinueMigration
    , projectCreateFromTemplate
    , projectCreateMigration
    , projectDelete
    , projectDocumentsView
    , projectFiles
    , projectImporters
    , projectMetrics
    , projectOpen
    , projectPreview
    , projectSettings
    , projectTagging
    , projectTemplatesCreate
    , projectTodos
    , projectVersionHistory
    , projectsCreateCustom
    , projectsCreateFromTemplate
    , projectsView
    , registry
    , settings
    , tenants
    , userEdit
    , userEditActiveSessions
    , userEditApiKeys
    , userEditAppKeys
    , userEditSubmissionSettings
    , usersCreate
    , usersView
    )

import Maybe.Extra as Maybe
import Shared.Auth.Permission as Perm
import Shared.Common.UuidOrCurrent as UuidOrCurrent exposing (UuidOrCurrent)
import Shared.Data.BootstrapConfig.Admin as Admin
import Shared.Data.Branch as Branch exposing (Branch)
import Shared.Data.Branch.BranchState as BranchState
import Shared.Data.Document as Document exposing (Document)
import Shared.Data.Document.DocumentState exposing (DocumentState(..))
import Shared.Data.Package.PackagePhase as PackagePhase exposing (PackagePhase)
import Shared.Data.Questionnaire as Questionnaire exposing (Questionnaire)
import Shared.Data.Questionnaire.QuestionnaireCreation as QuestionnaireCreation
import Shared.Data.Questionnaire.QuestionnaireState as QuestionnaireState
import Shared.Data.QuestionnaireDetail.Comment as Comment exposing (Comment)
import Shared.Data.QuestionnaireDetail.CommentThread as CommentThread exposing (CommentThread)
import Shared.Data.UserInfo as UserInfo
import Uuid
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.QuestionnaireUtils as QuestionnaireUtils exposing (QuestionnaireLike)



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


knowledgeModelSetDeprecated : AppState -> { a | phase : PackagePhase } -> Bool
knowledgeModelSetDeprecated appState package =
    adminOr Perm.packageManagementWrite appState
        && (package.phase == PackagePhase.Released)


knowledgeModelRestore : AppState -> { a | phase : PackagePhase } -> Bool
knowledgeModelRestore appState package =
    adminOr Perm.packageManagementWrite appState
        && (package.phase == PackagePhase.Deprecated)



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


projectCreateFromTemplate : AppState -> Questionnaire -> Bool
projectCreateFromTemplate appState questionnaire =
    projectsCreateFromTemplate appState && questionnaire.isTemplate && questionnaire.state /= QuestionnaireState.Migrating


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
    Questionnaire.isOwner appState questionnaire


projectTagging : AppState -> Bool
projectTagging appState =
    appState.config.questionnaire.projectTagging.enabled


projectMetrics : AppState -> Bool
projectMetrics appState =
    appState.config.questionnaire.summaryReport.enabled


projectPreview : AppState -> Bool
projectPreview _ =
    True


projectDocumentsView : AppState -> Bool
projectDocumentsView _ =
    True


projectTodos : AppState -> QuestionnaireLike q -> Bool
projectTodos appState questionnaire =
    QuestionnaireUtils.isEditor appState questionnaire && not (QuestionnaireUtils.isMigrating questionnaire)


projectVersionHistory : AppState -> QuestionnaireLike q -> Bool
projectVersionHistory appState questionnaire =
    QuestionnaireUtils.isEditor appState questionnaire && not (QuestionnaireUtils.isMigrating questionnaire)


projectSettings : AppState -> QuestionnaireLike q -> Bool
projectSettings appState questionnaire =
    QuestionnaireUtils.isOwner appState questionnaire


projectCommentAdd : AppState -> QuestionnaireLike q -> Bool
projectCommentAdd appState questionnaire =
    QuestionnaireUtils.canComment appState questionnaire


projectCommentEdit : AppState -> QuestionnaireLike q -> CommentThread -> Comment -> Bool
projectCommentEdit appState questionnaire commentThread comment =
    QuestionnaireUtils.canComment appState questionnaire && not commentThread.resolved && Comment.isAuthor appState.config.user comment


projectCommentDelete : AppState -> QuestionnaireLike q -> CommentThread -> Comment -> Bool
projectCommentDelete appState questionnaire commentThread comment =
    QuestionnaireUtils.canComment appState questionnaire && not commentThread.resolved && Comment.isAuthor appState.config.user comment


projectCommentThreadResolve : AppState -> QuestionnaireLike q -> CommentThread -> Bool
projectCommentThreadResolve appState questionnaire commentThread =
    QuestionnaireUtils.canComment appState questionnaire && not commentThread.resolved


projectCommentThreadAssign : AppState -> QuestionnaireLike q -> CommentThread -> Bool
projectCommentThreadAssign appState questionnaire commentThread =
    QuestionnaireUtils.canComment appState questionnaire && not (CommentThread.isAssigned commentThread)


projectCommentThreadRemoveAssign : AppState -> QuestionnaireLike q -> CommentThread -> Bool
projectCommentThreadRemoveAssign appState questionnaire commentThread =
    QuestionnaireUtils.canComment appState questionnaire && CommentThread.isAssigned commentThread


projectCommentThreadReopen : AppState -> QuestionnaireLike q -> CommentThread -> Bool
projectCommentThreadReopen appState questionnaire commentThread =
    QuestionnaireUtils.canComment appState questionnaire && commentThread.resolved


projectCommentThreadDelete : AppState -> QuestionnaireLike q -> CommentThread -> Bool
projectCommentThreadDelete appState questionnaire commentThread =
    QuestionnaireUtils.canComment appState questionnaire && CommentThread.isAuthor appState.config.user commentThread


projectCommentPrivate : AppState -> QuestionnaireLike q -> Bool
projectCommentPrivate appState questionnaire =
    QuestionnaireUtils.isEditor appState questionnaire



-- Project Files


projectFiles : AppState -> Bool
projectFiles =
    adminOr Perm.questionnaireFile



-- Project Actions


projectActions : AppState -> Bool
projectActions =
    adminOr Perm.questionnaireAction



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


registry : AppState -> Bool
registry appState =
    not (Admin.isEnabled appState.config.admin)



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


userEditApiKeys : AppState -> UuidOrCurrent -> Bool
userEditApiKeys appState uuidOrCurrent =
    UuidOrCurrent.isCurrent uuidOrCurrent || UuidOrCurrent.matchUuid uuidOrCurrent (Maybe.unwrap Uuid.nil .uuid appState.config.user)


userEditAppKeys : AppState -> UuidOrCurrent -> Bool
userEditAppKeys appState uuidOrCurrent =
    Admin.isEnabled appState.config.admin
        && (UuidOrCurrent.isCurrent uuidOrCurrent || UuidOrCurrent.matchUuid uuidOrCurrent (Maybe.unwrap Uuid.nil .uuid appState.config.user))


userEditActiveSessions : AppState -> UuidOrCurrent -> Bool
userEditActiveSessions appState uuidOrCurrent =
    UuidOrCurrent.isCurrent uuidOrCurrent || UuidOrCurrent.matchUuid uuidOrCurrent (Maybe.unwrap Uuid.nil .uuid appState.config.user)


userEditSubmissionSettings : AppState -> UuidOrCurrent -> Bool
userEditSubmissionSettings appState uuidOrCurrent =
    UuidOrCurrent.isCurrent uuidOrCurrent || UuidOrCurrent.matchUuid uuidOrCurrent (Maybe.unwrap Uuid.nil .uuid appState.config.user)



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



-- Tenants


tenants : AppState -> Bool
tenants appState =
    Perm.hasPerm appState.config.user Perm.tenants



-- Dev


dev : AppState -> Bool
dev appState =
    Perm.hasPerm appState.config.user Perm.dev



-- Helpers


isDataSteward : AppState -> Bool
isDataSteward appState =
    UserInfo.isDataSteward appState.config.user


isAdmin : AppState -> Bool
isAdmin appState =
    UserInfo.isAdmin appState.config.user


adminOr : String -> AppState -> Bool
adminOr perm appState =
    isAdmin appState || Perm.hasPerm appState.config.user perm
