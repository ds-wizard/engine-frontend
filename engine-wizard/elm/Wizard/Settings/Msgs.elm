module Wizard.Settings.Msgs exposing (Msg(..))

import Wizard.Settings.Generic.Msgs
import Wizard.Settings.Submission.Msgs


type Msg
    = AuthenticationMsg Wizard.Settings.Generic.Msgs.Msg
    | OrganizationMsg Wizard.Settings.Generic.Msgs.Msg
    | PrivacyAndSupportMsg Wizard.Settings.Generic.Msgs.Msg
    | DashboardMsg Wizard.Settings.Generic.Msgs.Msg
    | LookAndFeelMsg Wizard.Settings.Generic.Msgs.Msg
    | KnowledgeModelRegistryMsg Wizard.Settings.Generic.Msgs.Msg
    | QuestionnairesMsg Wizard.Settings.Generic.Msgs.Msg
    | SubmissionMsg Wizard.Settings.Submission.Msgs.Msg
