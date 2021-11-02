module Wizard.Settings.Msgs exposing (Msg(..))

import Wizard.Settings.Generic.Msgs
import Wizard.Settings.LookAndFeel.Msgs
import Wizard.Settings.Registry.Msgs
import Wizard.Settings.Submission.Msgs
import Wizard.Settings.Template.Msgs


type Msg
    = AuthenticationMsg Wizard.Settings.Generic.Msgs.Msg
    | OrganizationMsg Wizard.Settings.Generic.Msgs.Msg
    | PrivacyAndSupportMsg Wizard.Settings.Generic.Msgs.Msg
    | DashboardMsg Wizard.Settings.Generic.Msgs.Msg
    | LookAndFeelMsg Wizard.Settings.LookAndFeel.Msgs.Msg
    | RegistryMsg Wizard.Settings.Registry.Msgs.Msg
    | QuestionnairesMsg Wizard.Settings.Generic.Msgs.Msg
    | SubmissionMsg Wizard.Settings.Submission.Msgs.Msg
    | TemplateMsg Wizard.Settings.Template.Msgs.Msg
    | KnowledgeModelsMsg Wizard.Settings.Generic.Msgs.Msg
