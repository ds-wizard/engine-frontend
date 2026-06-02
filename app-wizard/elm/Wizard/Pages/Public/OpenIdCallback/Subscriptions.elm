module Wizard.Pages.Public.OpenIdCallback.Subscriptions exposing (subscriptions)

import Common.Ports.LocalStorage as LocalStorage
import Json.Decode as D
import Wizard.Pages.Public.OpenIdCallback.Msgs exposing (Msg(..))


subscriptions : Sub Msg
subscriptions =
    LocalStorage.gotItem (D.maybe D.string) GotOriginalUrl
