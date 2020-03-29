module Wizard.Settings.LookAndFeel.Update exposing (update)

import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Config.LookAndFeelConfig as LookAndFeelConfig exposing (LookAndFeelConfig)
import Wizard.Msgs
import Wizard.Settings.Common.EditableConfig as EditableConfig
import Wizard.Settings.Generic.Msgs exposing (Msg)
import Wizard.Settings.Generic.Update as GenericUpdate
import Wizard.Settings.LookAndFeel.Models exposing (Model)


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update =
    GenericUpdate.update updateProps


updateProps : GenericUpdate.UpdateProps LookAndFeelConfig
updateProps =
    { initForm = .lookAndFeel >> LookAndFeelConfig.initForm
    , formToConfig = EditableConfig.updateLookAndFeel
    , formValidation = LookAndFeelConfig.validation
    }
