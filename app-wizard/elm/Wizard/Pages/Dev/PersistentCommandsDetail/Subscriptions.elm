module Wizard.Pages.Dev.PersistentCommandsDetail.Subscriptions exposing (subscriptions)

import Bootstrap.Dropdown as Dropdown
import Wizard.Pages.Dev.PersistentCommandsDetail.Models exposing (Model)
import Wizard.Pages.Dev.PersistentCommandsDetail.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Dropdown.subscriptions model.dropdownState DropdownMsg
