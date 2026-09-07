module Wizard.Pages.Users.Edit.UserEditRoutes exposing (UserEditRoute(..))

import Uuid exposing (Uuid)


type UserEditRoute
    = Profile
    | Password
    | ConnectedAccounts
    | Language
    | Tours
    | ApiKeys
    | ActiveSessions
    | SubmissionSettings
    | PluginSettings Uuid
