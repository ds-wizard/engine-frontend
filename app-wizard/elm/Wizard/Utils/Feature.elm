module Wizard.Utils.Feature exposing
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
    , knowledgeModelSecrets
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
    , newsModal
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
    , projectSearch
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
    , urlChecker
    , userEdit
    , userEditActiveSessions
    , userEditApiKeys
    , userEditAppKeys
    , userEditLanguage
    , userEditSubmissionSettings
    , userEditTours
    , usersCreate
    , usersView
    )

import Common.Api.Models.UserInfo as UserInfo
import Common.Data.UuidOrCurrent as UuidOrCurrent exposing (UuidOrCurrent)
import Maybe.Extra as Maybe
import Uuid
import Wizard.Api.Models.BootstrapConfig.Admin as Admin
import Wizard.Api.Models.Document as Document exposing (Document)
import Wizard.Api.Models.Document.DocumentState exposing (DocumentState(..))
import Wizard.Api.Models.KnowledgeModelEditor as KnowledgeModelEditor exposing (KnowledgeModelEditor)
import Wizard.Api.Models.KnowledgeModelEditor.KnowledgeModelEditorState as KnowledgeModelEditorState
import Wizard.Api.Models.KnowledgeModelPackage.KnowledgeModelPackagePhase as KnowledgeModelPackagePhase exposing (KnowledgeModelPackagePhase)
import Wizard.Api.Models.Project as Project exposing (Project)
import Wizard.Api.Models.Project.ProjectCreation as ProjectCreation
import Wizard.Api.Models.Project.ProjectState as ProjectState
import Wizard.Api.Models.ProjectDetail.Comment as Comment exposing (Comment)
import Wizard.Api.Models.ProjectDetail.CommentThread as CommentThread exposing (CommentThread)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Data.Perm as Perm
import Wizard.Data.Session as Session
import Wizard.Utils.ProjectUtils as ProjectUtils exposing (ProjectLike)



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


knowledgeModelEditorOpen : AppState -> KnowledgeModelEditor -> Bool
knowledgeModelEditorOpen appState knowledgeModelEditor =
    adminOr Perm.knowledgeModel appState
        && KnowledgeModelEditor.matchState [ KnowledgeModelEditorState.Default, KnowledgeModelEditorState.Edited, KnowledgeModelEditorState.Outdated ] knowledgeModelEditor


knowledgeModelEditorPublish : AppState -> KnowledgeModelEditor -> Bool
knowledgeModelEditorPublish appState knowledgeModelEditor =
    adminOr Perm.knowledgeModelPublish appState
        && KnowledgeModelEditor.matchState [ KnowledgeModelEditorState.Edited, KnowledgeModelEditorState.Migrated ] knowledgeModelEditor


knowledgeModelEditorUpgrade : AppState -> KnowledgeModelEditor -> Bool
knowledgeModelEditorUpgrade appState knowledgeModelEditor =
    adminOr Perm.knowledgeModelUpgrade appState
        && KnowledgeModelEditor.matchState [ KnowledgeModelEditorState.Outdated ] knowledgeModelEditor


knowledgeModelEditorContinueMigration : AppState -> KnowledgeModelEditor -> Bool
knowledgeModelEditorContinueMigration appState knowledgeModelEditor =
    adminOr Perm.knowledgeModelUpgrade appState
        && KnowledgeModelEditor.matchState [ KnowledgeModelEditorState.Migrating ] knowledgeModelEditor


knowledgeModelEditorCancelMigration : AppState -> KnowledgeModelEditor -> Bool
knowledgeModelEditorCancelMigration appState knowledgeModelEditor =
    adminOr Perm.knowledgeModelUpgrade appState
        && KnowledgeModelEditor.matchState [ KnowledgeModelEditorState.Migrating, KnowledgeModelEditorState.Migrated ] knowledgeModelEditor


knowledgeModelEditorDelete : AppState -> KnowledgeModelEditor -> Bool
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


knowledgeModelSetDeprecated : AppState -> { a | phase : KnowledgeModelPackagePhase } -> Bool
knowledgeModelSetDeprecated appState kmPackage =
    adminOr Perm.packageManagementWrite appState
        && (kmPackage.phase == KnowledgeModelPackagePhase.Released)


knowledgeModelRestore : AppState -> { a | phase : KnowledgeModelPackagePhase } -> Bool
knowledgeModelRestore appState kmPackage =
    adminOr Perm.packageManagementWrite appState
        && (kmPackage.phase == KnowledgeModelPackagePhase.Deprecated)



-- Knowledge Model Secrets


knowledgeModelSecrets : AppState -> Bool
knowledgeModelSecrets appState =
    adminOr Perm.knowledgeModel appState



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



-- News


newsModal : AppState -> Bool
newsModal appState =
    isAdmin appState || isDataSteward appState



-- Projects


projectsView : AppState -> Bool
projectsView =
    adminOr Perm.project


projectsCreateCustom : AppState -> Bool
projectsCreateCustom appState =
    let
        canCreateCustomProjects =
            ProjectCreation.customEnabled appState.config.project.projectCreation

        canCreateProjectTemplates =
            adminOr Perm.projectTemplate appState

        canCreateAnonymousProjects =
            appState.config.project.projectSharing.anonymousEnabled
    in
    (canCreateAnonymousProjects || adminOr Perm.project appState) && (canCreateCustomProjects || canCreateProjectTemplates)


projectsCreateFromTemplate : AppState -> Bool
projectsCreateFromTemplate appState =
    let
        canCreateFromTemplates =
            ProjectCreation.fromTemplateEnabled appState.config.project.projectCreation
    in
    adminOr Perm.project appState && canCreateFromTemplates


projectTemplatesCreate : AppState -> Bool
projectTemplatesCreate =
    adminOr Perm.projectTemplate


projectOpen : AppState -> Project -> Bool
projectOpen _ project =
    project.state /= ProjectState.Migrating


projectCreateFromTemplate : AppState -> Project -> Bool
projectCreateFromTemplate appState project =
    projectsCreateFromTemplate appState && project.isTemplate && project.state /= ProjectState.Migrating


projectClone : AppState -> Project -> Bool
projectClone _ project =
    project.state /= ProjectState.Migrating


projectCreateMigration : AppState -> Project -> Bool
projectCreateMigration appState project =
    Project.isEditable appState project && project.state /= ProjectState.Migrating


projectContinueMigration : AppState -> Project -> Bool
projectContinueMigration appState project =
    Project.isEditable appState project && project.state == ProjectState.Migrating


projectCancelMigration : AppState -> Project -> Bool
projectCancelMigration appState project =
    Project.isEditable appState project && project.state == ProjectState.Migrating


projectDelete : AppState -> Project -> Bool
projectDelete appState project =
    Project.isOwner appState project


projectTagging : AppState -> Bool
projectTagging appState =
    appState.config.project.projectTagging.enabled


projectMetrics : AppState -> Bool
projectMetrics appState =
    appState.config.project.summaryReport.enabled


projectPreview : AppState -> Bool
projectPreview _ =
    True


projectDocumentsView : AppState -> Bool
projectDocumentsView _ =
    True


projectSearch : AppState -> ProjectLike q -> Bool
projectSearch _ project =
    not (ProjectUtils.isMigrating project)


projectTodos : AppState -> ProjectLike q -> Bool
projectTodos appState project =
    ProjectUtils.isEditor appState project && not (ProjectUtils.isMigrating project)


projectVersionHistory : AppState -> ProjectLike q -> Bool
projectVersionHistory appState project =
    ProjectUtils.isEditor appState project && not (ProjectUtils.isMigrating project)


projectSettings : AppState -> ProjectLike q -> Bool
projectSettings appState project =
    ProjectUtils.isOwner appState project


projectCommentAdd : AppState -> ProjectLike q -> Bool
projectCommentAdd appState project =
    ProjectUtils.canComment appState project


projectCommentEdit : AppState -> ProjectLike q -> CommentThread -> Comment -> Bool
projectCommentEdit appState project commentThread comment =
    ProjectUtils.canComment appState project && not commentThread.resolved && Comment.isAuthor appState.config.user comment


projectCommentDelete : AppState -> ProjectLike q -> CommentThread -> Comment -> Bool
projectCommentDelete appState project commentThread comment =
    ProjectUtils.canComment appState project && not commentThread.resolved && Comment.isAuthor appState.config.user comment


projectCommentThreadResolve : AppState -> ProjectLike q -> CommentThread -> Bool
projectCommentThreadResolve appState project commentThread =
    ProjectUtils.canComment appState project && not commentThread.resolved


projectCommentThreadAssign : AppState -> ProjectLike q -> CommentThread -> Bool
projectCommentThreadAssign appState project commentThread =
    Session.exists appState.session && ProjectUtils.canComment appState project && not (CommentThread.isAssigned commentThread)


projectCommentThreadRemoveAssign : AppState -> ProjectLike q -> CommentThread -> Bool
projectCommentThreadRemoveAssign appState project commentThread =
    ProjectUtils.canComment appState project && CommentThread.isAssigned commentThread


projectCommentThreadReopen : AppState -> ProjectLike q -> CommentThread -> Bool
projectCommentThreadReopen appState project commentThread =
    ProjectUtils.canComment appState project && commentThread.resolved


projectCommentThreadDelete : AppState -> ProjectLike q -> CommentThread -> Bool
projectCommentThreadDelete appState project commentThread =
    ProjectUtils.canComment appState project && CommentThread.isAuthor appState.config.user commentThread


projectCommentPrivate : AppState -> ProjectLike q -> Bool
projectCommentPrivate appState project =
    ProjectUtils.isEditor appState project



-- Project Files


projectFiles : AppState -> Bool
projectFiles =
    adminOr Perm.projectFile



-- Project Actions


projectActions : AppState -> Bool
projectActions =
    adminOr Perm.projectAction



-- Project Importers


projectImporters : AppState -> Bool
projectImporters =
    adminOr Perm.projectImporter



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


userEditLanguage : AppState -> UuidOrCurrent -> Bool
userEditLanguage appState uuidOrCurrent =
    UuidOrCurrent.isCurrent uuidOrCurrent || UuidOrCurrent.matchUuid uuidOrCurrent (Maybe.unwrap Uuid.nil .uuid appState.config.user)


userEditTours : AppState -> UuidOrCurrent -> Bool
userEditTours appState uuidOrCurrent =
    UuidOrCurrent.isCurrent uuidOrCurrent || UuidOrCurrent.matchUuid uuidOrCurrent (Maybe.unwrap Uuid.nil .uuid appState.config.user)


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
    appState.config.submission.enabled && (UuidOrCurrent.isCurrent uuidOrCurrent || UuidOrCurrent.matchUuid uuidOrCurrent (Maybe.unwrap Uuid.nil .uuid appState.config.user))



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
    String.startsWith "~" locale.organizationId


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



-- Other


urlChecker : AppState -> Bool
urlChecker appState =
    Maybe.isJust appState.urlCheckerUrl



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
