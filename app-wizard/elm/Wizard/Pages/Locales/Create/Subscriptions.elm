module Wizard.Pages.Locales.Create.Subscriptions exposing (subscriptions)

import Common.Ports.Locale as Locale
import Wizard.Pages.Locales.Create.Msgs exposing (Msg(..))


subscriptions : Sub Msg
subscriptions =
    Locale.localeConverted LocaleConverted
