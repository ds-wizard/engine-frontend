module Wizard.Settings.Features.Update exposing (update)

import Wizard.Api.Models.EditableConfig as EditableConfig
import Wizard.Api.Models.EditableConfig.EditableFeaturesConfig as EditableFeaturesConfig exposing (EditableFeaturesConfig)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Settings.Features.Models exposing (Model)
import Wizard.Settings.Generic.Msgs exposing (Msg)
import Wizard.Settings.Generic.Update as GenericUpdate


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update =
    GenericUpdate.update updateProps


updateProps : GenericUpdate.UpdateProps EditableFeaturesConfig
updateProps =
    { initForm = .features >> EditableFeaturesConfig.initForm
    , formToConfig = EditableConfig.updateFeatures
    , formValidation = EditableFeaturesConfig.validation
    }
