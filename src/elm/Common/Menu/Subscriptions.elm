module Common.Menu.Subscriptions exposing (..)

import Bootstrap.Dropdown as Dropdown
import Common.Menu.Models exposing (Model)
import Common.Menu.Msgs exposing (Msg(ProfileMenuDropdownMsg))
import Msgs


subscriptions : Model -> Sub Msgs.Msg
subscriptions model =
    Dropdown.subscriptions model.profileMenuDropdownState (Msgs.MenuMsg << ProfileMenuDropdownMsg)
