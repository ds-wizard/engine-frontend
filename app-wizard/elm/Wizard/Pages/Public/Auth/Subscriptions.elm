module Wizard.Pages.Public.Auth.Subscriptions exposing (subscriptions)

import Wizard.Pages.Public.Auth.Msgs exposing (Msg(..))
import Wizard.Ports as Ports


subscriptions : Sub Msg
subscriptions =
    Ports.localStorageData GotOriginalUrl
