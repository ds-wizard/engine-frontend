module Wizard.Pages.Settings.LookAndFeel.Update exposing (update)

import Wizard.Api.Models.EditableConfig as EditableConfig
import Wizard.Api.Models.EditableConfig.EditableLookAndFeelConfig as EditableLookAndFeelConfig exposing (EditableLookAndFeelConfig)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.Settings.Generic.Msgs exposing (Msg)
import Wizard.Pages.Settings.Generic.Update as GenericUpdate
import Wizard.Pages.Settings.LookAndFeel.Models exposing (Model)


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update =
    GenericUpdate.update updateProps


updateProps : GenericUpdate.UpdateProps EditableLookAndFeelConfig
updateProps =
    { initForm = .lookAndFeel >> EditableLookAndFeelConfig.initForm
    , formToConfig = EditableConfig.updateLookAndFeel
    , formValidation = EditableLookAndFeelConfig.validation
    }
