module Wizard.Pages.Locales.Create.Subscriptions exposing (subscriptions)

import Common.Ports.Locale as Locale
import Wizard.Pages.Locales.Create.Models exposing (Model)
import Wizard.Pages.Locales.Create.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions _ =
    Locale.localeConverted LocaleConverted
