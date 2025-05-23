module Wizard.Users.Edit.Msgs exposing (Msg(..))

import Wizard.Users.Edit.Components.ActiveSessions as ActiveSessions
import Wizard.Users.Edit.Components.ApiKeys as ApiKeys
import Wizard.Users.Edit.Components.AppKeys as AppKeys
import Wizard.Users.Edit.Components.Language as Language
import Wizard.Users.Edit.Components.Password as Password
import Wizard.Users.Edit.Components.Profile as Profile
import Wizard.Users.Edit.Components.SubmissionSettings as SubmissionSettings
import Wizard.Users.Edit.Components.Tours as Tours


type Msg
    = ProfileMsg Profile.Msg
    | PasswordMsg Password.Msg
    | LanguageMsg Language.Msg
    | ToursMsg Tours.Msg
    | ApiKeysMsg ApiKeys.Msg
    | AppKeysMsg AppKeys.Msg
    | ActiveSessionsMsg ActiveSessions.Msg
    | SubmissionSettingsMsg SubmissionSettings.Msg
