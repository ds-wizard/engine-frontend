module Wizard.Pages.Settings.Msgs exposing (Msg(..))

import Wizard.Pages.Settings.Authentication.Msgs
import Wizard.Pages.Settings.Generic.Msgs
import Wizard.Pages.Settings.PluginSettings.Msgs
import Wizard.Pages.Settings.Plugins.Msgs
import Wizard.Pages.Settings.Registry.Msgs
import Wizard.Pages.Settings.Submission.Msgs
import Wizard.Pages.Settings.Usage.Msgs


type Msg
    = AuthenticationMsg Wizard.Pages.Settings.Authentication.Msgs.Msg
    | OrganizationMsg Wizard.Pages.Settings.Generic.Msgs.Msg
    | PrivacyAndSupportMsg Wizard.Pages.Settings.Generic.Msgs.Msg
    | FeaturesMsg Wizard.Pages.Settings.Generic.Msgs.Msg
    | PluginsMsg Wizard.Pages.Settings.Plugins.Msgs.Msg
    | PluginSettingsMsg Wizard.Pages.Settings.PluginSettings.Msgs.Msg
    | DashboardMsg Wizard.Pages.Settings.Generic.Msgs.Msg
    | LookAndFeelMsg Wizard.Pages.Settings.Generic.Msgs.Msg
    | RegistryMsg Wizard.Pages.Settings.Registry.Msgs.Msg
    | QuestionnairesMsg Wizard.Pages.Settings.Generic.Msgs.Msg
    | SubmissionMsg Wizard.Pages.Settings.Submission.Msgs.Msg
    | KnowledgeModelsMsg Wizard.Pages.Settings.Generic.Msgs.Msg
    | UsageMsg Wizard.Pages.Settings.Usage.Msgs.Msg
