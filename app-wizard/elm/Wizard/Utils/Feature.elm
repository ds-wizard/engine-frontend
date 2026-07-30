module Wizard.Utils.Feature exposing
    ( LocaleLike
    , dev
    , documentDelete
    , documentDownload
    , documentSubmit
    , documentTemplateEditorsCreate
    , documentTemplateEditorsDelete
    , documentTemplateEditorsEdit
    , documentTemplateEditorsPublish
    , documentTemplateEditorsView
    , documentTemplatesDelete
    , documentTemplatesExport
    , documentTemplatesImport
    , documentTemplatesManage
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
    , knowledgeModelRestore
    , knowledgeModelSecrets
    , knowledgeModelSetDeprecated
    , knowledgeModelSetPrivate
    , knowledgeModelSetPublic
    , knowledgeModelsDelete
    , knowledgeModelsDeleteLocale
    , knowledgeModelsExport
    , knowledgeModelsExportLocale
    , knowledgeModelsExportPot
    , knowledgeModelsImport
    , knowledgeModelsImportLocale
    , knowledgeModelsManage
    , knowledgeModelsPreview
    , knowledgeModelsView
    , localeChangeEnabled
    , localeCreate
    , localeDelete
    , localeExport
    , localeImport
    , localeSetDefault
    , localeView
    , localesManage
    , newsModal
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
    , projectMetrics
    , projectOpen
    , projectPreview
    , projectSearch
    , projectSettings
    , projectTagging
    , projectTemplatesCreate
    , projectTodos
    , projectToolbarImporters
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
    , userEditConnectedAccounts
    , userEditLanguage
    , userEditPlugins
    , userEditSubmissionSettings
    , userEditTours
    , usersCreate
    , usersManage
    , usersView
    )

import Common.Api.Models.RolePermission exposing (RolePermission)
import Common.Data.UuidOrCurrent as UuidOrCurrent exposing (UuidOrCurrent)
import Common.Data.WizardRolePermission as RolePermission
import Maybe.Extra as Maybe
import Uuid
import Wizard.Api.Models.BootstrapConfig.AdminConfig as Admin
import Wizard.Api.Models.BootstrapConfig.UserConfig as UserConfig
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
import Wizard.Data.Session as Session
import Wizard.Utils.ProjectUtils as ProjectUtils exposing (ProjectLike)



-- Knowledge Model Editors


knowledgeModelEditorsView : AppState -> Bool
knowledgeModelEditorsView =
    hasPerm RolePermission.knowledgeModelEditorsUse


knowledgeModelEditorsCreate : AppState -> Bool
knowledgeModelEditorsCreate =
    hasPerm RolePermission.knowledgeModelEditorsUse


knowledgeModelEditorsEdit : AppState -> Bool
knowledgeModelEditorsEdit =
    hasPerm RolePermission.knowledgeModelEditorsUse


knowledgeModelEditorsUpgrade : AppState -> Bool
knowledgeModelEditorsUpgrade =
    hasPerm RolePermission.knowledgeModelEditorsUse


knowledgeModelEditorsPublish : AppState -> Bool
knowledgeModelEditorsPublish =
    hasPerm RolePermission.knowledgeModelEditorsUse


knowledgeModelEditorOpen : AppState -> KnowledgeModelEditor -> Bool
knowledgeModelEditorOpen appState knowledgeModelEditor =
    hasPerm RolePermission.knowledgeModelEditorsUse appState
        && KnowledgeModelEditor.matchState [ KnowledgeModelEditorState.Default, KnowledgeModelEditorState.Edited, KnowledgeModelEditorState.Outdated ] knowledgeModelEditor


knowledgeModelEditorPublish : AppState -> KnowledgeModelEditor -> Bool
knowledgeModelEditorPublish appState knowledgeModelEditor =
    hasPerm RolePermission.knowledgeModelEditorsUse appState
        && KnowledgeModelEditor.matchState [ KnowledgeModelEditorState.Edited, KnowledgeModelEditorState.Migrated ] knowledgeModelEditor


knowledgeModelEditorUpgrade : AppState -> KnowledgeModelEditor -> Bool
knowledgeModelEditorUpgrade appState knowledgeModelEditor =
    hasPerm RolePermission.knowledgeModelEditorsUse appState
        && KnowledgeModelEditor.matchState [ KnowledgeModelEditorState.Outdated ] knowledgeModelEditor


knowledgeModelEditorContinueMigration : AppState -> KnowledgeModelEditor -> Bool
knowledgeModelEditorContinueMigration appState knowledgeModelEditor =
    hasPerm RolePermission.knowledgeModelEditorsUse appState
        && KnowledgeModelEditor.matchState [ KnowledgeModelEditorState.Migrating ] knowledgeModelEditor


knowledgeModelEditorCancelMigration : AppState -> KnowledgeModelEditor -> Bool
knowledgeModelEditorCancelMigration appState knowledgeModelEditor =
    hasPerm RolePermission.knowledgeModelEditorsUse appState
        && KnowledgeModelEditor.matchState [ KnowledgeModelEditorState.Migrating, KnowledgeModelEditorState.Migrated ] knowledgeModelEditor


knowledgeModelEditorDelete : AppState -> Bool
knowledgeModelEditorDelete =
    hasPerm RolePermission.knowledgeModelEditorsUse



-- Knowledge Models


knowledgeModelsView : AppState -> Bool
knowledgeModelsView =
    isLoggedIn


knowledgeModelsManage : AppState -> Bool
knowledgeModelsManage =
    hasPerm RolePermission.knowledgeModelsManage


knowledgeModelsImport : AppState -> Bool
knowledgeModelsImport =
    hasPerm RolePermission.knowledgeModelsManage


knowledgeModelsImportLocale : AppState -> Bool
knowledgeModelsImportLocale =
    hasPerm RolePermission.knowledgeModelsManage


knowledgeModelsExport : AppState -> Bool
knowledgeModelsExport =
    hasPerm RolePermission.knowledgeModelsManage


knowledgeModelsExportLocale : AppState -> Bool
knowledgeModelsExportLocale =
    hasPerm RolePermission.knowledgeModelsManage


knowledgeModelsExportPot : AppState -> Bool
knowledgeModelsExportPot =
    hasPerm RolePermission.knowledgeModelsManage


knowledgeModelsDelete : AppState -> Bool
knowledgeModelsDelete =
    hasPerm RolePermission.knowledgeModelsManage


knowledgeModelsDeleteLocale : AppState -> Bool
knowledgeModelsDeleteLocale =
    hasPerm RolePermission.knowledgeModelsManage


knowledgeModelsPreview : Bool
knowledgeModelsPreview =
    True


knowledgeModelSetDeprecated : AppState -> { a | phase : KnowledgeModelPackagePhase } -> Bool
knowledgeModelSetDeprecated appState kmPackage =
    hasPerm RolePermission.knowledgeModelsManage appState
        && (kmPackage.phase == KnowledgeModelPackagePhase.Released)


knowledgeModelRestore : AppState -> { a | phase : KnowledgeModelPackagePhase } -> Bool
knowledgeModelRestore appState kmPackage =
    hasPerm RolePermission.knowledgeModelsManage appState
        && (kmPackage.phase == KnowledgeModelPackagePhase.Deprecated)


knowledgeModelSetPublic : AppState -> { a | public : Bool } -> Bool
knowledgeModelSetPublic appState kmPackage =
    hasPerm RolePermission.knowledgeModelsManage appState
        && not kmPackage.public


knowledgeModelSetPrivate : AppState -> { a | public : Bool } -> Bool
knowledgeModelSetPrivate appState kmPackage =
    hasPerm RolePermission.knowledgeModelsManage appState
        && kmPackage.public



-- Knowledge Model Secrets


knowledgeModelSecrets : AppState -> Bool
knowledgeModelSecrets =
    hasPerm RolePermission.knowledgeModelsManage



-- Document Templates


documentTemplatesView : AppState -> Bool
documentTemplatesView =
    isLoggedIn


documentTemplatesImport : AppState -> Bool
documentTemplatesImport =
    hasPerm RolePermission.documentTemplatesManage


documentTemplatesExport : AppState -> Bool
documentTemplatesExport =
    hasPerm RolePermission.documentTemplatesManage


documentTemplatesDelete : AppState -> Bool
documentTemplatesDelete =
    hasPerm RolePermission.documentTemplatesManage



-- Document Template Editors


documentTemplateEditorsView : AppState -> Bool
documentTemplateEditorsView =
    hasPerm RolePermission.documentTemplateEditorsUse


documentTemplatesManage : AppState -> Bool
documentTemplatesManage =
    hasPerm RolePermission.documentTemplatesManage


documentTemplateEditorsCreate : AppState -> Bool
documentTemplateEditorsCreate =
    hasPerm RolePermission.documentTemplateEditorsUse


documentTemplateEditorsEdit : AppState -> Bool
documentTemplateEditorsEdit =
    hasPerm RolePermission.documentTemplateEditorsUse


documentTemplateEditorsPublish : AppState -> Bool
documentTemplateEditorsPublish =
    hasPerm RolePermission.documentTemplateEditorsUse


documentTemplateEditorsDelete : AppState -> Bool
documentTemplateEditorsDelete =
    hasPerm RolePermission.documentTemplateEditorsUse



-- News


newsModal : AppState -> Bool
newsModal appState =
    hasPerm RolePermission.knowledgeModelEditorsUse appState
        || hasPerm RolePermission.settingsManage appState



-- Projects


projectsView : AppState -> Bool
projectsView =
    always True


projectsCreateCustom : AppState -> Bool
projectsCreateCustom appState =
    let
        canCreateCustomProjects =
            ProjectCreation.customEnabled appState.config.project.projectCreation

        canCreateProjectTemplates =
            hasPerm RolePermission.projectTemplatesManage appState

        canCreateAnonymousProjects =
            appState.config.project.projectSharing.anonymousEnabled
    in
    (canCreateAnonymousProjects || isLoggedIn appState) && (canCreateCustomProjects || canCreateProjectTemplates)


projectsCreateFromTemplate : AppState -> Bool
projectsCreateFromTemplate appState =
    let
        canCreateFromTemplates =
            ProjectCreation.fromTemplateEnabled appState.config.project.projectCreation
    in
    isLoggedIn appState && canCreateFromTemplates


projectTemplatesCreate : AppState -> Bool
projectTemplatesCreate =
    hasPerm RolePermission.projectTemplatesManage


projectOpen : Project -> Bool
projectOpen project =
    project.state /= ProjectState.Migrating


projectCreateFromTemplate : AppState -> Project -> Bool
projectCreateFromTemplate appState project =
    projectsCreateFromTemplate appState && project.isTemplate && project.state /= ProjectState.Migrating


projectClone : Project -> Bool
projectClone project =
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


projectPreview : Bool
projectPreview =
    True


projectDocumentsView : Bool
projectDocumentsView =
    True


projectSearch : ProjectLike q -> Bool
projectSearch project =
    not (ProjectUtils.isMigrating project)


projectTodos : AppState -> ProjectLike q -> Bool
projectTodos appState project =
    ProjectUtils.isEditor appState project && not (ProjectUtils.isMigrating project)


projectToolbarImporters : AppState -> ProjectLike q -> Bool
projectToolbarImporters appState project =
    Session.exists appState.session && ProjectUtils.isEditor appState project && not (ProjectUtils.isMigrating project)


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
    hasPerm RolePermission.projectsEdit



-- Documents


documentsView : AppState -> Bool
documentsView =
    hasPerm RolePermission.projectsEdit


documentDelete : AppState -> Document -> Bool
documentDelete appState document =
    hasPerm RolePermission.projectsEdit appState || Document.isOwner appState document


documentDownload : Document -> Bool
documentDownload document =
    document.state == DoneDocumentState


documentSubmit : AppState -> Document -> Bool
documentSubmit appState document =
    (document.state == DoneDocumentState)
        && appState.config.submission.enabled
        && hasPerm RolePermission.projectsEdit appState



-- Settings


settings : AppState -> Bool
settings =
    hasPerm RolePermission.settingsManage


registry : AppState -> Bool
registry appState =
    not (Admin.isEnabled appState.config.admin)



-- Users


usersCreate : AppState -> Bool
usersCreate =
    hasPerm RolePermission.usersManage


userEdit : AppState -> UuidOrCurrent -> Bool
userEdit appState uuidOrCurrent =
    UuidOrCurrent.isCurrent uuidOrCurrent || hasPerm RolePermission.usersManage appState


usersManage : AppState -> Bool
usersManage =
    hasPerm RolePermission.usersManage


usersView : AppState -> Bool
usersView =
    hasPerm RolePermission.usersManage


userEditConnectedAccounts : AppState -> UuidOrCurrent -> Bool
userEditConnectedAccounts appState uuidOrCurrent =
    let
        anyExternalServices =
            not (List.isEmpty appState.config.authentication.external.services)
    in
    anyExternalServices && (UuidOrCurrent.isCurrent uuidOrCurrent || UuidOrCurrent.matchUuid uuidOrCurrent (Maybe.unwrap Uuid.nil .uuid appState.config.user))


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


userEditPlugins : AppState -> UuidOrCurrent -> Bool
userEditPlugins appState uuidOrCurrent =
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
    String.startsWith "~" locale.organizationId


localeView : AppState -> Bool
localeView =
    hasPerm RolePermission.settingsManage


localeCreate : AppState -> Bool
localeCreate =
    hasPerm RolePermission.settingsManage


localeImport : AppState -> Bool
localeImport =
    hasPerm RolePermission.settingsManage


localeExport : AppState -> LocaleLike a -> Bool
localeExport appState locale =
    hasPerm RolePermission.settingsManage appState
        && not (isDefaultLanguage locale)


localeSetDefault : AppState -> LocaleLike a -> Bool
localeSetDefault appState locale =
    hasPerm RolePermission.settingsManage appState
        && locale.enabled
        && not locale.defaultLocale


localeChangeEnabled : AppState -> LocaleLike a -> Bool
localeChangeEnabled appState locale =
    hasPerm RolePermission.settingsManage appState
        && not locale.defaultLocale


localeDelete : AppState -> LocaleLike a -> Bool
localeDelete appState locale =
    hasPerm RolePermission.settingsManage appState
        && not (isDefaultLanguage locale)
        && not locale.defaultLocale


localesManage : AppState -> Bool
localesManage =
    hasPerm RolePermission.settingsManage



-- Tenants


tenants : AppState -> Bool
tenants =
    hasPerm RolePermission.tenantsManage



-- Other


urlChecker : AppState -> Bool
urlChecker appState =
    Maybe.isJust appState.urlCheckerUrl



-- Dev


dev : AppState -> Bool
dev =
    hasPerm RolePermission.devUse



-- Helpers


hasPerm : RolePermission -> AppState -> Bool
hasPerm perm appState =
    Maybe.unwrap False (UserConfig.hasPerm perm) appState.config.user


isLoggedIn : AppState -> Bool
isLoggedIn appState =
    Maybe.isJust appState.config.user
