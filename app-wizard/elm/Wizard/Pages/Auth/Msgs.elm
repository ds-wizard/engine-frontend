module Wizard.Pages.Auth.Msgs exposing (Msg(..))

import Shared.Data.Token exposing (Token)
import Wizard.Routes exposing (Route)


type Msg
    = Logout
    | LogoutTo Route
    | LogoutDone
    | GotToken Token (Maybe String)
