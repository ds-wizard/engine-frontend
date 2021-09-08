module Wizard.Common.Feature exposing
    ( documentDelete
    , documentDownload
    , documentSubmit
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
    , knowledgeModelsDelete
    , knowledgeModelsExport
    , knowledgeModelsImport
    , knowledgeModelsPreview
    , knowledgeModelsView
    , projectCancelMigration
    , projectClone
    , projectContinueMigration
    , projectCreateMigration
    , projectDelete
    , projectDocumentsView
    , projectMetrics
    , projectOpen
    , projectPreview
    , projectSettings
    , projectTemplatesCreate
    , projectsCreateCustom
    , projectsCreateFromTemplate
    , projectsView
    , settings
    , templatesDelete
    , templatesExport
    , templatesImport
    , templatesView
    , userEdit
    , usersCreate
    , usersView
    )

import Shared.Auth.Permission as Perm
import Shared.Data.Branch as Branch exposing (Branch)
import Shared.Data.Branch.BranchState as BranchState
import Shared.Data.Document as Document exposing (Document)
import Shared.Data.Document.DocumentState exposing (DocumentState(..))
import Shared.Data.Questionnaire as Questionnaire exposing (Questionnaire)
import Shared.Data.Questionnaire.QuestionnaireCreation as QuestionnaireCreation
import Shared.Data.Questionnaire.QuestionnaireState as QuestionnaireState
import Shared.Data.QuestionnaireDetail as QuestionnaireDetail exposing (QuestionnaireDetail)
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



-- Templates


templatesView : AppState -> Bool
templatesView =
    adminOr Perm.templates


templatesImport : AppState -> Bool
templatesImport =
    adminOr Perm.packageManagementWrite


templatesExport : AppState -> Bool
templatesExport =
    adminOr Perm.templates


templatesDelete : AppState -> Bool
templatesDelete =
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


projectMetrics : AppState -> QuestionnaireDetail -> Bool
projectMetrics appState _ =
    appState.config.questionnaire.summaryReport.enabled


projectPreview : AppState -> QuestionnaireDetail -> Bool
projectPreview _ _ =
    True


projectDocumentsView : AppState -> QuestionnaireDetail -> Bool
projectDocumentsView _ _ =
    True


projectSettings : AppState -> QuestionnaireDetail -> Bool
projectSettings appState questionnaire =
    QuestionnaireDetail.isOwner appState questionnaire



-- Documents


documentsView : AppState -> Bool
documentsView appState =
    adminOr Perm.dataManagementPlan appState


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


userEdit : AppState -> String -> Bool
userEdit appState uuid =
    (uuid == "current") || adminOr Perm.userManagement appState



-- Helpers


isAdmin : AppState -> Bool
isAdmin appState =
    UserInfo.isAdmin appState.session.user


adminOr : String -> AppState -> Bool
adminOr perm appState =
    isAdmin appState || Perm.hasPerm appState.session perm
