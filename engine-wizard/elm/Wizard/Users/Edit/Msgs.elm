module Wizard.Users.Edit.Msgs exposing (Msg(..))

import Wizard.Users.Edit.Components.ActiveSessions as ActiveSessions
import Wizard.Users.Edit.Components.ApiKeys as ApiKeys
import Wizard.Users.Edit.Components.Password as Password
import Wizard.Users.Edit.Components.Profile as Profile
import Wizard.Users.Edit.Components.SubmissionSettings as SubmissionSettings


type Msg
    = ProfileMsg Profile.Msg
    | PasswordMsg Password.Msg
    | ApiKeysMsg ApiKeys.Msg
    | ActiveSessionsMsg ActiveSessions.Msg
    | SubmissionSettingsMsg SubmissionSettings.Msg
