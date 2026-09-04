module Wizard.Pages.Users.Edit.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Wizard.Api.Models.User exposing (User)
import Wizard.Pages.Users.Edit.Components.ActiveSessions as ActiveSessions
import Wizard.Pages.Users.Edit.Components.ApiKeys as ApiKeys
import Wizard.Pages.Users.Edit.Components.AppKeys as AppKeys
import Wizard.Pages.Users.Edit.Components.ConnectedAccounts as ConnectedAccounts
import Wizard.Pages.Users.Edit.Components.Language as Language
import Wizard.Pages.Users.Edit.Components.Password as Password
import Wizard.Pages.Users.Edit.Components.PluginSettings as PluginSettings
import Wizard.Pages.Users.Edit.Components.Profile as Profile
import Wizard.Pages.Users.Edit.Components.SubmissionSettings as SubmissionSettings
import Wizard.Pages.Users.Edit.Components.Tours as Tours


type Msg
    = GetUserCompleted (Result ApiError User)
    | ProfileMsg Profile.Msg
    | PasswordMsg Password.Msg
    | ConnectedAccountsMsg ConnectedAccounts.Msg
    | LanguageMsg Language.Msg
    | ToursMsg Tours.Msg
    | ApiKeysMsg ApiKeys.Msg
    | AppKeysMsg AppKeys.Msg
    | ActiveSessionsMsg ActiveSessions.Msg
    | SubmissionSettingsMsg SubmissionSettings.Msg
    | PluginSettingsMsg PluginSettings.Msg
