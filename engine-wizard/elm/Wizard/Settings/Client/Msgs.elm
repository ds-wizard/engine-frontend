module Wizard.Settings.Client.Msgs exposing (Msg)

import Wizard.Settings.Common.EditableClientConfig exposing (EditableClientConfig)
import Wizard.Settings.Generic.Msgs as GenericMsgs


type alias Msg =
    GenericMsgs.Msg EditableClientConfig
