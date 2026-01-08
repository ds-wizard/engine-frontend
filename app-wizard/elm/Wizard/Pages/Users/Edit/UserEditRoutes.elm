module Wizard.Pages.Users.Edit.UserEditRoutes exposing (UserEditRoute(..))

import Uuid exposing (Uuid)


type UserEditRoute
    = Profile
    | Password
    | Language
    | Tours
    | ApiKeys
    | AppKeys
    | ActiveSessions
    | SubmissionSettings
    | PluginSettings Uuid
