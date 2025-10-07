module Wizard.Pages.Auth.Msgs exposing (Msg(..))

import Common.Api.Models.Token exposing (Token)
import Wizard.Routes exposing (Route)


type Msg
    = Logout
    | LogoutTo Route
    | LogoutDone
    | GotToken Token (Maybe String)
