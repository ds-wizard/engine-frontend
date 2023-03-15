module Wizard.DocumentTemplates.Detail.Subscriptions exposing (subscriptions)

import Bootstrap.Dropdown as Dropdown
import Wizard.DocumentTemplates.Detail.Models exposing (Model)
import Wizard.DocumentTemplates.Detail.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Dropdown.subscriptions model.dropdownState DropdownMsg
