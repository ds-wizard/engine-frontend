module Wizard.Public.Auth.Subscriptions exposing (subscriptions)

import Wizard.Ports as Ports
import Wizard.Public.Auth.Msgs exposing (Msg(..))


subscriptions : Sub Msg
subscriptions =
    Ports.localStorageData GotOriginalUrl
