module Wizard.Settings.LookAndFeel.Update exposing (update)

import Shared.Data.BootstrapConfig.LookAndFeelConfig as LookAndFeelConfig exposing (LookAndFeelConfig)
import Shared.Data.EditableConfig as EditableConfig
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
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
