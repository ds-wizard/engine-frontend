module Wizard.Questionnaires.Detail.Subscriptions exposing (..)

import Bootstrap.Dropdown as Dropdown
import Wizard.Questionnaires.Detail.Models exposing (Model)
import Wizard.Questionnaires.Detail.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Dropdown.subscriptions model.actionsDropdownState ActionsDropdownMsg
