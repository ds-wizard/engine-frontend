module Common.Menu.Subscriptions exposing (subscriptions)

import Bootstrap.Dropdown as Dropdown
import Common.Menu.Models exposing (Model)
import Common.Menu.Msgs exposing (Msg(..))
import Msgs


subscriptions : Model -> Sub Msgs.Msg
subscriptions model =
    Sub.batch
        [ Dropdown.subscriptions model.helpMenuDropdownState (Msgs.MenuMsg << HelpMenuDropdownMsg)
        , Dropdown.subscriptions model.profileMenuDropdownState (Msgs.MenuMsg << ProfileMenuDropdownMsg)
        ]
