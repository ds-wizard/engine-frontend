module Wizard.Settings.Features.Msgs exposing (Msg)

import Wizard.Settings.Common.EditableFeaturesConfig exposing (EditableFeaturesConfig)
import Wizard.Settings.Generic.Msgs as GenericMsgs


type alias Msg =
    GenericMsgs.Msg EditableFeaturesConfig
