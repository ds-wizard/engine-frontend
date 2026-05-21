module Wizard.Pages.Settings.Msgs exposing (Msg(..))

import Wizard.Pages.Settings.Generic.Msgs
import Wizard.Pages.Settings.OpenId.Msgs
import Wizard.Pages.Settings.OpenIdCreate.Msgs
import Wizard.Pages.Settings.OpenIdDetail.Msgs
import Wizard.Pages.Settings.PluginSettings.Msgs
import Wizard.Pages.Settings.Plugins.Msgs
import Wizard.Pages.Settings.Registry.Msgs
import Wizard.Pages.Settings.Submission.Msgs
import Wizard.Pages.Settings.Usage.Msgs


type Msg
    = AuthenticationMsg Wizard.Pages.Settings.Generic.Msgs.Msg
    | OrganizationMsg Wizard.Pages.Settings.Generic.Msgs.Msg
    | OpenIdMsg Wizard.Pages.Settings.OpenId.Msgs.Msg
    | OpenIdCreateMsg Wizard.Pages.Settings.OpenIdCreate.Msgs.Msg
    | OpenIdDetailMsg Wizard.Pages.Settings.OpenIdDetail.Msgs.Msg
    | PrivacyAndSupportMsg Wizard.Pages.Settings.Generic.Msgs.Msg
    | FeaturesMsg Wizard.Pages.Settings.Generic.Msgs.Msg
    | PluginsMsg Wizard.Pages.Settings.Plugins.Msgs.Msg
    | PluginSettingsMsg Wizard.Pages.Settings.PluginSettings.Msgs.Msg
    | DashboardMsg Wizard.Pages.Settings.Generic.Msgs.Msg
    | LookAndFeelMsg Wizard.Pages.Settings.Generic.Msgs.Msg
    | RegistryMsg Wizard.Pages.Settings.Registry.Msgs.Msg
    | QuestionnairesMsg Wizard.Pages.Settings.Generic.Msgs.Msg
    | SubmissionMsg Wizard.Pages.Settings.Submission.Msgs.Msg
    | UsageMsg Wizard.Pages.Settings.Usage.Msgs.Msg
