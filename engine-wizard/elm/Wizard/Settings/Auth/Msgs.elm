module Wizard.Settings.Auth.Msgs exposing (Msg)

import Wizard.Settings.Common.EditableAuthConfig exposing (EditableAuthConfig)
import Wizard.Settings.Generic.Msgs as GenericMsgs


type alias Msg =
    GenericMsgs.Msg EditableAuthConfig
