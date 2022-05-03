module Wizard.Common.Menu.Subscriptions exposing (subscriptions)

import Bootstrap.Dropdown as Dropdown
import Wizard.Common.Menu.Models exposing (Model)
import Wizard.Common.Menu.Msgs exposing (Msg(..))
import Wizard.Msgs


subscriptions : Model -> Sub Wizard.Msgs.Msg
subscriptions model =
    Sub.batch
        [ Dropdown.subscriptions model.helpMenuDropdownState (Wizard.Msgs.MenuMsg << HelpMenuDropdownMsg)
        , Dropdown.subscriptions model.devMenuDropdownState (Wizard.Msgs.MenuMsg << DevMenuDropdownMsg)
        , Dropdown.subscriptions model.profileMenuDropdownState (Wizard.Msgs.MenuMsg << ProfileMenuDropdownMsg)
        ]
