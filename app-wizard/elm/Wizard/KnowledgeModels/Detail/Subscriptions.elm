module Wizard.KnowledgeModels.Detail.Subscriptions exposing (subscriptions)

import Bootstrap.Dropdown as Dropdown
import Wizard.KnowledgeModels.Detail.Models exposing (Model)
import Wizard.KnowledgeModels.Detail.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Dropdown.subscriptions model.dropdownState DropdownMsg
