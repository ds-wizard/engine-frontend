module Wizard.Locales.Create.Subscriptions exposing (subscriptions)

import Wizard.Locales.Create.Models exposing (Model)
import Wizard.Locales.Create.Msgs exposing (Msg(..))
import Wizard.Ports as Ports


subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.localeConverted LocaleConverted
