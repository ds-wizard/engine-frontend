module Wizard.Pages.Auth.Msgs exposing (Msg(..))

import Common.Api.Models.Token exposing (Token)


type Msg
    = Logout
    | LogoutToLogin (Maybe String)
    | LogoutDone (Maybe String)
    | GotToken Token (Maybe String)
