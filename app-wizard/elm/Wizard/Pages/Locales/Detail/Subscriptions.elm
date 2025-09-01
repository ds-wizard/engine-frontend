module Wizard.Pages.Locales.Detail.Subscriptions exposing (subscriptions)

import Bootstrap.Dropdown as Dropdown
import Wizard.Pages.Locales.Detail.Models exposing (Model)
import Wizard.Pages.Locales.Detail.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Dropdown.subscriptions model.dropdownState DropdownMsg
