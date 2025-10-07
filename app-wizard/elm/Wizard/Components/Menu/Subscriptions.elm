module Wizard.Components.Menu.Subscriptions exposing (subscriptions)

import Bootstrap.Dropdown as Dropdown
import Wizard.Components.Menu.Models exposing (Model)
import Wizard.Components.Menu.Msgs exposing (Msg(..))
import Wizard.Msgs


subscriptions : Model -> Sub Wizard.Msgs.Msg
subscriptions model =
    Sub.batch
        [ Dropdown.subscriptions model.helpMenuDropdownState (Wizard.Msgs.MenuMsg << HelpMenuDropdownMsg)
        , Dropdown.subscriptions model.devMenuDropdownState (Wizard.Msgs.MenuMsg << DevMenuDropdownMsg)
        , Dropdown.subscriptions model.profileMenuDropdownState (Wizard.Msgs.MenuMsg << ProfileMenuDropdownMsg)
        ]
