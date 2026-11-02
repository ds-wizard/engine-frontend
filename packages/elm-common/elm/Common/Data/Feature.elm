module Common.Data.Feature exposing
    ( AppStateLike
    , AppStateWithUrlChecker
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
    , documentTemplatesDeleteLocale
    , documentTemplatesExport
    , documentTemplatesExportLocale
    , documentTemplatesExportPot
    , documentTemplatesImport
    , documentTemplatesImportLocale
    , documentTemplatesManage
    , documentTemplatesView
    , documentsView
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
    , localesManage
    , newsModal
    , projectCommentAdd
    , projectCommentDelete
    , projectCommentEdit
    , projectCommentPrivate
    , projectCommentThreadAssign
    , projectCommentThreadDelete
    , projectCommentThreadRemoveAssign
    , projectCommentThreadReopen
    , projectCommentThreadResolve
    , projectCreateFromTemplate
    , projectCreateMigration
    , projectDelete
    , projectDocumentSubmit
    , projectDocumentsView
    , projectFiles
    , projectMetrics
    , projectPreview
    , projectSettings
    , projectTagging
    , projectTemplatesCreate
    , projectTodos
    , projectToolbarImporters
    , projectVersionHistory
    , projectsCreate
    , projectsCreateCustom
    , projectsCreateFromTemplate
    , projectsView
    , registry
    , roles
    , settings
    , settingsAny
    , tenants
    , urlChecker
    , userEdit
    , userEditActiveSessions
    , userEditApiKeys
    , userEditConnectedAccounts
    , userEditLanguage
    , userEditPlugins
    , userEditSubmissionSettings
    , userEditTours
    , userGroupEdit
    , userGroupsCreate
    , userGroupsView
    , usersCreate
    , usersManage
    , usersView
    , workspaceSettings
    )

import Common.Api.Models.BootstrapConfig exposing (BootstrapConfig)
import Common.Api.Models.BootstrapConfig.UserConfig as UserConfig
import Common.Api.Models.Document as Document exposing (Document)
import Common.Api.Models.Document.DocumentState exposing (DocumentState(..))
import Common.Api.Models.KnowledgeModelEditor as KnowledgeModelEditor exposing (KnowledgeModelEditor)
import Common.Api.Models.KnowledgeModelEditor.KnowledgeModelEditorState as KnowledgeModelEditorState
import Common.Api.Models.KnowledgeModelPackage.KnowledgeModelPackagePhase as KnowledgeModelPackagePhase exposing (KnowledgeModelPackagePhase)
import Common.Api.Models.Project exposing (Project)
import Common.Api.Models.Project.ProjectCreation as ProjectCreation
import Common.Api.Models.ProjectDetail.Comment as Comment exposing (Comment)
import Common.Api.Models.ProjectDetail.CommentThread as CommentThread exposing (CommentThread)
import Common.Api.Models.RolePermission exposing (RolePermission)
import Common.Api.Models.UserGroupMember exposing (UserGroupMember)
import Common.Api.Models.UserGroupMembershipType exposing (UserGroupMembershipType(..))
import Common.Data.ProjectUtils as ProjectUtils exposing (ProjectLike)
import Common.Data.Session as Session exposing (Session)
import Common.Data.UuidOrCurrent as UuidOrCurrent exposing (UuidOrCurrent)
import Common.Data.WizardRolePermission as RolePermission
import Maybe.Extra as Maybe
import Uuid


type alias AppStateLike a =
    { a
        | config : BootstrapConfig
        , session : Session
    }


type alias AppStateWithUrlChecker a =
    AppStateLike { a | urlCheckerUrl : Maybe String }



-- Knowledge Model Editors


knowledgeModelEditorsView : AppStateLike a -> Bool
knowledgeModelEditorsView =
    hasPerm RolePermission.knowledgeModelEditorsUse


knowledgeModelEditorsCreate : AppStateLike a -> Bool
knowledgeModelEditorsCreate =
    hasPerm RolePermission.knowledgeModelEditorsUse


knowledgeModelEditorsEdit : AppStateLike a -> Bool
knowledgeModelEditorsEdit =
    hasPerm RolePermission.knowledgeModelEditorsUse


knowledgeModelEditorsUpgrade : AppStateLike a -> Bool
knowledgeModelEditorsUpgrade =
    hasPerm RolePermission.knowledgeModelEditorsUse


knowledgeModelEditorsPublish : AppStateLike a -> Bool
knowledgeModelEditorsPublish =
    hasPerm RolePermission.knowledgeModelEditorsUse


knowledgeModelEditorOpen : AppStateLike a -> KnowledgeModelEditor -> Bool
knowledgeModelEditorOpen appState knowledgeModelEditor =
    hasPerm RolePermission.knowledgeModelEditorsUse appState
        && KnowledgeModelEditor.matchState [ KnowledgeModelEditorState.Default, KnowledgeModelEditorState.Edited, KnowledgeModelEditorState.Outdated ] knowledgeModelEditor


knowledgeModelEditorPublish : AppStateLike a -> KnowledgeModelEditor -> Bool
knowledgeModelEditorPublish appState knowledgeModelEditor =
    hasPerm RolePermission.knowledgeModelEditorsUse appState
        && KnowledgeModelEditor.matchState [ KnowledgeModelEditorState.Edited, KnowledgeModelEditorState.Migrated ] knowledgeModelEditor


knowledgeModelEditorUpgrade : AppStateLike a -> KnowledgeModelEditor -> Bool
knowledgeModelEditorUpgrade appState knowledgeModelEditor =
    hasPerm RolePermission.knowledgeModelEditorsUse appState
        && KnowledgeModelEditor.matchState [ KnowledgeModelEditorState.Outdated ] knowledgeModelEditor


knowledgeModelEditorContinueMigration : AppStateLike a -> KnowledgeModelEditor -> Bool
knowledgeModelEditorContinueMigration appState knowledgeModelEditor =
    hasPerm RolePermission.knowledgeModelEditorsUse appState
        && KnowledgeModelEditor.matchState [ KnowledgeModelEditorState.Migrating ] knowledgeModelEditor


knowledgeModelEditorCancelMigration : AppStateLike a -> KnowledgeModelEditor -> Bool
knowledgeModelEditorCancelMigration appState knowledgeModelEditor =
    hasPerm RolePermission.knowledgeModelEditorsUse appState
        && KnowledgeModelEditor.matchState [ KnowledgeModelEditorState.Migrating, KnowledgeModelEditorState.Migrated ] knowledgeModelEditor


knowledgeModelEditorDelete : AppStateLike a -> Bool
knowledgeModelEditorDelete =
    hasPerm RolePermission.knowledgeModelEditorsUse



-- Knowledge Models


knowledgeModelsView : AppStateLike a -> Bool
knowledgeModelsView =
    isLoggedIn


knowledgeModelsManage : AppStateLike a -> Bool
knowledgeModelsManage =
    hasPerm RolePermission.knowledgeModelsManage


knowledgeModelsImport : AppStateLike a -> Bool
knowledgeModelsImport =
    hasPerm RolePermission.knowledgeModelsManage


knowledgeModelsImportLocale : AppStateLike a -> Bool
knowledgeModelsImportLocale =
    hasPerm RolePermission.knowledgeModelsManage


knowledgeModelsExport : AppStateLike a -> Bool
knowledgeModelsExport =
    hasPerm RolePermission.knowledgeModelsManage


knowledgeModelsExportLocale : AppStateLike a -> Bool
knowledgeModelsExportLocale =
    hasPerm RolePermission.knowledgeModelsManage


knowledgeModelsExportPot : AppStateLike a -> Bool
knowledgeModelsExportPot =
    hasPerm RolePermission.knowledgeModelsManage


knowledgeModelsDelete : AppStateLike a -> Bool
knowledgeModelsDelete =
    hasPerm RolePermission.knowledgeModelsManage


knowledgeModelsDeleteLocale : AppStateLike a -> Bool
knowledgeModelsDeleteLocale =
    hasPerm RolePermission.knowledgeModelsManage


knowledgeModelsPreview : Bool
knowledgeModelsPreview =
    True


knowledgeModelSetDeprecated : AppStateLike a -> { b | phase : KnowledgeModelPackagePhase } -> Bool
knowledgeModelSetDeprecated appState kmPackage =
    hasPerm RolePermission.knowledgeModelsManage appState
        && (kmPackage.phase == KnowledgeModelPackagePhase.Released)


knowledgeModelRestore : AppStateLike a -> { b | phase : KnowledgeModelPackagePhase } -> Bool
knowledgeModelRestore appState kmPackage =
    hasPerm RolePermission.knowledgeModelsManage appState
        && (kmPackage.phase == KnowledgeModelPackagePhase.Deprecated)


knowledgeModelSetPublic : AppStateLike a -> { b | public : Bool } -> Bool
knowledgeModelSetPublic appState kmPackage =
    hasPerm RolePermission.knowledgeModelsManage appState
        && not kmPackage.public


knowledgeModelSetPrivate : AppStateLike a -> { b | public : Bool } -> Bool
knowledgeModelSetPrivate appState kmPackage =
    hasPerm RolePermission.knowledgeModelsManage appState
        && kmPackage.public



-- Knowledge Model Secrets


knowledgeModelSecrets : AppStateLike a -> Bool
knowledgeModelSecrets =
    hasPerm RolePermission.knowledgeModelsManage



-- Document Templates


documentTemplatesView : AppStateLike a -> Bool
documentTemplatesView =
    isLoggedIn


documentTemplatesImport : AppStateLike a -> Bool
documentTemplatesImport =
    hasPerm RolePermission.documentTemplatesManage


documentTemplatesImportLocale : AppStateLike a -> Bool
documentTemplatesImportLocale =
    hasPerm RolePermission.documentTemplatesManage


documentTemplatesExport : AppStateLike a -> Bool
documentTemplatesExport =
    hasPerm RolePermission.documentTemplatesManage


documentTemplatesExportLocale : AppStateLike a -> Bool
documentTemplatesExportLocale =
    hasPerm RolePermission.documentTemplatesManage


documentTemplatesExportPot : AppStateLike a -> Bool
documentTemplatesExportPot =
    hasPerm RolePermission.documentTemplatesManage


documentTemplatesDelete : AppStateLike a -> Bool
documentTemplatesDelete =
    hasPerm RolePermission.documentTemplatesManage


documentTemplatesDeleteLocale : AppStateLike a -> Bool
documentTemplatesDeleteLocale =
    hasPerm RolePermission.documentTemplatesManage



-- Document Template Editors


documentTemplateEditorsView : AppStateLike a -> Bool
documentTemplateEditorsView =
    hasPerm RolePermission.documentTemplateEditorsUse


documentTemplatesManage : AppStateLike a -> Bool
documentTemplatesManage =
    hasPerm RolePermission.documentTemplatesManage


documentTemplateEditorsCreate : AppStateLike a -> Bool
documentTemplateEditorsCreate =
    hasPerm RolePermission.documentTemplateEditorsUse


documentTemplateEditorsEdit : AppStateLike a -> Bool
documentTemplateEditorsEdit =
    hasPerm RolePermission.documentTemplateEditorsUse


documentTemplateEditorsPublish : AppStateLike a -> Bool
documentTemplateEditorsPublish =
    hasPerm RolePermission.documentTemplateEditorsUse


documentTemplateEditorsDelete : AppStateLike a -> Bool
documentTemplateEditorsDelete =
    hasPerm RolePermission.documentTemplateEditorsUse



-- News


newsModal : AppStateLike a -> Bool
newsModal appState =
    hasPerm RolePermission.knowledgeModelEditorsUse appState
        || hasPerm RolePermission.settingsManage appState



-- Projects


projectsView : AppStateLike a -> Bool
projectsView =
    always True


{-| Somebody can start a project one way or the other: what decides whether
"Create" is offered at all.
-}
projectsCreate : AppStateLike a -> Bool
projectsCreate appState =
    projectsCreateCustom appState || projectsCreateFromTemplate appState


projectsCreateCustom : AppStateLike a -> Bool
projectsCreateCustom appState =
    let
        canCreateCustomProjects =
            ProjectCreation.customEnabled appState.config.project.projectCreation

        canCreateProjectTemplates =
            hasPerm RolePermission.projectTemplatesManage appState

        canCreateAnonymousProjects =
            not (isLoggedIn appState) && appState.config.project.projectSharing.anonymousEnabled
    in
    (canCreateAnonymousProjects || canCreateProjects appState) && (canCreateCustomProjects || canCreateProjectTemplates)


projectsCreateFromTemplate : AppStateLike a -> Bool
projectsCreateFromTemplate appState =
    let
        canCreateFromTemplates =
            ProjectCreation.fromTemplateEnabled appState.config.project.projectCreation
    in
    canCreateProjects appState && canCreateFromTemplates


{-| Somebody signed in whose role lets them start projects.
-}
canCreateProjects : AppStateLike a -> Bool
canCreateProjects appState =
    isLoggedIn appState && hasPerm RolePermission.projectsCreate appState


projectTemplatesCreate : AppStateLike a -> Bool
projectTemplatesCreate =
    hasPerm RolePermission.projectTemplatesManage


projectCreateFromTemplate : AppStateLike a -> Project -> Bool
projectCreateFromTemplate appState project =
    projectsCreateFromTemplate appState && project.isTemplate


projectCreateMigration : AppStateLike a -> ProjectLike q -> Bool
projectCreateMigration =
    ProjectUtils.isOwner


projectDelete : AppStateLike a -> Project -> Bool
projectDelete =
    ProjectUtils.isOwner


projectTagging : AppStateLike a -> Bool
projectTagging appState =
    appState.config.project.projectTagging.enabled


projectMetrics : AppStateLike a -> Bool
projectMetrics appState =
    appState.config.project.summaryReport.enabled


projectPreview : Bool
projectPreview =
    True


projectDocumentsView : Bool
projectDocumentsView =
    True


projectDocumentSubmit : AppStateLike a -> ProjectLike q -> Document -> Bool
projectDocumentSubmit appState project document =
    (document.state == DoneDocumentState)
        && appState.config.submission.enabled
        && Session.exists appState.session
        && ProjectUtils.isEditor appState project


projectTodos : AppStateLike a -> ProjectLike q -> Bool
projectTodos appState project =
    ProjectUtils.isEditor appState project


projectToolbarImporters : AppStateLike a -> ProjectLike q -> Bool
projectToolbarImporters appState project =
    Session.exists appState.session && ProjectUtils.isEditor appState project


projectVersionHistory : AppStateLike a -> ProjectLike q -> Bool
projectVersionHistory appState project =
    ProjectUtils.isEditor appState project


projectSettings : AppStateLike a -> ProjectLike q -> Bool
projectSettings appState project =
    ProjectUtils.isOwner appState project


projectCommentAdd : AppStateLike a -> ProjectLike q -> Bool
projectCommentAdd appState project =
    ProjectUtils.canComment appState project


projectCommentEdit : AppStateLike a -> ProjectLike q -> CommentThread -> Comment -> Bool
projectCommentEdit appState project commentThread comment =
    ProjectUtils.canComment appState project && not commentThread.resolved && Comment.isAuthor appState.config.user comment


projectCommentDelete : AppStateLike a -> ProjectLike q -> CommentThread -> Comment -> Bool
projectCommentDelete appState project commentThread comment =
    ProjectUtils.canComment appState project && not commentThread.resolved && Comment.isAuthor appState.config.user comment


projectCommentThreadResolve : AppStateLike a -> ProjectLike q -> CommentThread -> Bool
projectCommentThreadResolve appState project commentThread =
    ProjectUtils.canComment appState project && not commentThread.resolved


projectCommentThreadAssign : AppStateLike a -> ProjectLike q -> CommentThread -> Bool
projectCommentThreadAssign appState project commentThread =
    Session.exists appState.session && ProjectUtils.canComment appState project && not (CommentThread.isAssigned commentThread)


projectCommentThreadRemoveAssign : AppStateLike a -> ProjectLike q -> CommentThread -> Bool
projectCommentThreadRemoveAssign appState project commentThread =
    ProjectUtils.canComment appState project && CommentThread.isAssigned commentThread


projectCommentThreadReopen : AppStateLike a -> ProjectLike q -> CommentThread -> Bool
projectCommentThreadReopen appState project commentThread =
    ProjectUtils.canComment appState project && commentThread.resolved


projectCommentThreadDelete : AppStateLike a -> ProjectLike q -> CommentThread -> Bool
projectCommentThreadDelete appState project commentThread =
    ProjectUtils.canComment appState project && CommentThread.isAuthor appState.config.user commentThread


projectCommentPrivate : AppStateLike a -> ProjectLike q -> Bool
projectCommentPrivate appState project =
    ProjectUtils.isEditor appState project



-- Project Files


projectFiles : AppStateLike a -> Bool
projectFiles =
    hasPerm RolePermission.projectsEdit



-- Documents


documentsView : AppStateLike a -> Bool
documentsView =
    hasPerm RolePermission.projectsEdit


documentDelete : AppStateLike a -> Document -> Bool
documentDelete appState document =
    hasPerm RolePermission.projectsEdit appState || Document.isOwner appState document


documentDownload : Document -> Bool
documentDownload document =
    document.state == DoneDocumentState


documentSubmit : AppStateLike a -> Document -> Bool
documentSubmit appState document =
    (document.state == DoneDocumentState)
        && appState.config.submission.enabled
        && hasPerm RolePermission.projectsEdit appState



-- Settings


{-| Whether there is anything of the settings for the user: the settings,
the workspace settings, or the roles.
-}
settingsAny : AppStateLike a -> Bool
settingsAny appState =
    settings appState || workspaceSettings appState || roles appState


{-| The settings (Manage Settings): how people sign in, the users, the
features, the look, the login screen, the registry, the plugins and the usage.
-}
settings : AppStateLike a -> Bool
settings =
    hasPerm RolePermission.settingsManage


{-| The settings of projects and of what people see: projects, dashboard and
menu, support and submission (Manage Workspace Settings, or Manage General
Settings where there is one workspace). Never how people sign in.
-}
workspaceSettings : AppStateLike a -> Bool
workspaceSettings =
    hasPerm RolePermission.workspaceSettingsManage


{-| The roles and which of them is the default.
-}
roles : AppStateLike a -> Bool
roles =
    hasPerm RolePermission.rolesManage


registry : AppStateLike a -> Bool
registry =
    always True



-- User groups


{-| Anybody signed in sees the groups of a workspace they are in; the server
leaves out the private ones they are not in.
-}
userGroupsView : AppStateLike a -> Bool
userGroupsView =
    isLoggedIn


{-| Creating a group takes Create User Groups where it is made.
-}
userGroupsCreate : AppStateLike a -> Bool
userGroupsCreate appState =
    isLoggedIn appState && hasPerm RolePermission.userGroupsCreate appState


{-| A group is changed by its owners and by those who manage every group.
-}
userGroupEdit : AppStateLike a -> { b | users : List UserGroupMember } -> Bool
userGroupEdit appState group =
    let
        isOwner user =
            List.any (\member -> member.uuid == user.uuid && member.membershipType == Owner) group.users
    in
    hasPerm RolePermission.userGroupsManage appState || Maybe.unwrap False isOwner appState.config.user



-- Users


usersCreate : AppStateLike a -> Bool
usersCreate =
    hasPerm RolePermission.usersManage


userEdit : AppStateLike a -> UuidOrCurrent -> Bool
userEdit appState uuidOrCurrent =
    UuidOrCurrent.isCurrent uuidOrCurrent || hasPerm RolePermission.usersManage appState


usersManage : AppStateLike a -> Bool
usersManage =
    hasPerm RolePermission.usersManage


usersView : AppStateLike a -> Bool
usersView =
    hasPerm RolePermission.usersManage


userEditConnectedAccounts : AppStateLike a -> UuidOrCurrent -> Bool
userEditConnectedAccounts appState uuidOrCurrent =
    let
        anyExternalServices =
            not (List.isEmpty appState.config.authentication.services)
    in
    anyExternalServices && (UuidOrCurrent.isCurrent uuidOrCurrent || UuidOrCurrent.matchUuid uuidOrCurrent (Maybe.unwrap Uuid.nil .uuid appState.config.user))


userEditLanguage : AppStateLike a -> UuidOrCurrent -> Bool
userEditLanguage appState uuidOrCurrent =
    UuidOrCurrent.isCurrent uuidOrCurrent || UuidOrCurrent.matchUuid uuidOrCurrent (Maybe.unwrap Uuid.nil .uuid appState.config.user)


userEditTours : AppStateLike a -> UuidOrCurrent -> Bool
userEditTours appState uuidOrCurrent =
    UuidOrCurrent.isCurrent uuidOrCurrent || UuidOrCurrent.matchUuid uuidOrCurrent (Maybe.unwrap Uuid.nil .uuid appState.config.user)


userEditApiKeys : AppStateLike a -> UuidOrCurrent -> Bool
userEditApiKeys appState uuidOrCurrent =
    UuidOrCurrent.isCurrent uuidOrCurrent || UuidOrCurrent.matchUuid uuidOrCurrent (Maybe.unwrap Uuid.nil .uuid appState.config.user)


userEditActiveSessions : AppStateLike a -> UuidOrCurrent -> Bool
userEditActiveSessions appState uuidOrCurrent =
    UuidOrCurrent.isCurrent uuidOrCurrent || UuidOrCurrent.matchUuid uuidOrCurrent (Maybe.unwrap Uuid.nil .uuid appState.config.user)


userEditSubmissionSettings : AppStateLike a -> UuidOrCurrent -> Bool
userEditSubmissionSettings appState uuidOrCurrent =
    appState.config.submission.enabled && (UuidOrCurrent.isCurrent uuidOrCurrent || UuidOrCurrent.matchUuid uuidOrCurrent (Maybe.unwrap Uuid.nil .uuid appState.config.user))


userEditPlugins : AppStateLike a -> UuidOrCurrent -> Bool
userEditPlugins appState uuidOrCurrent =
    UuidOrCurrent.isCurrent uuidOrCurrent || UuidOrCurrent.matchUuid uuidOrCurrent (Maybe.unwrap Uuid.nil .uuid appState.config.user)



-- Locale


localesManage : AppStateLike a -> Bool
localesManage =
    hasPerm RolePermission.settingsManage



-- Tenants


tenants : AppStateLike a -> Bool
tenants =
    hasPerm RolePermission.tenantsManage



-- Other


urlChecker : AppStateWithUrlChecker a -> Bool
urlChecker appState =
    Maybe.isJust appState.urlCheckerUrl



-- Dev


dev : AppStateLike a -> Bool
dev =
    hasPerm RolePermission.devUse



-- Helpers


hasPerm : RolePermission -> AppStateLike a -> Bool
hasPerm perm appState =
    Maybe.unwrap False (UserConfig.hasPerm perm) appState.config.user


isLoggedIn : AppStateLike a -> Bool
isLoggedIn appState =
    Maybe.isJust appState.config.user
