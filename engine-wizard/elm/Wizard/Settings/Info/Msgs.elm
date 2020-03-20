module Wizard.Settings.Info.Msgs exposing (Msg)

import Wizard.Settings.Common.EditableInfoConfig exposing (EditableInfoConfig)
import Wizard.Settings.Generic.Msgs as GenericMsgs


type alias Msg =
    GenericMsgs.Msg EditableInfoConfig
