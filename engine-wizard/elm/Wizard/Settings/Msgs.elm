module Wizard.Settings.Msgs exposing (Msg(..))

import Wizard.Settings.Authentication.Msgs
import Wizard.Settings.Generic.Msgs
import Wizard.Settings.Plans.Msgs
import Wizard.Settings.Registry.Msgs
import Wizard.Settings.Submission.Msgs
import Wizard.Settings.Usage.Msgs


type Msg
    = AuthenticationMsg Wizard.Settings.Authentication.Msgs.Msg
    | OrganizationMsg Wizard.Settings.Generic.Msgs.Msg
    | PrivacyAndSupportMsg Wizard.Settings.Generic.Msgs.Msg
    | DashboardMsg Wizard.Settings.Generic.Msgs.Msg
    | LookAndFeelMsg Wizard.Settings.Generic.Msgs.Msg
    | RegistryMsg Wizard.Settings.Registry.Msgs.Msg
    | QuestionnairesMsg Wizard.Settings.Generic.Msgs.Msg
    | SubmissionMsg Wizard.Settings.Submission.Msgs.Msg
    | KnowledgeModelsMsg Wizard.Settings.Generic.Msgs.Msg
    | UsageMsg Wizard.Settings.Usage.Msgs.Msg
    | PlansMsg Wizard.Settings.Plans.Msgs.Msg
