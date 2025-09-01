module Wizard.Dev.PersistentCommandsDetail.Subscriptions exposing (subscriptions)

import Bootstrap.Dropdown as Dropdown
import Wizard.Dev.PersistentCommandsDetail.Models exposing (Model)
import Wizard.Dev.PersistentCommandsDetail.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Dropdown.subscriptions model.dropdownState DropdownMsg
