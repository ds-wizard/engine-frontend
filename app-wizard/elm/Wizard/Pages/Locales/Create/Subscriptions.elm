module Wizard.Pages.Locales.Create.Subscriptions exposing (subscriptions)

import Wizard.Pages.Locales.Create.Models exposing (Model)
import Wizard.Pages.Locales.Create.Msgs exposing (Msg(..))
import Wizard.Ports as Ports


subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.localeConverted LocaleConverted
