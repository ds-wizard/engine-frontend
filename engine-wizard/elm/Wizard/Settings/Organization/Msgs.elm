module Wizard.Settings.Organization.Msgs exposing (Msg)

import Wizard.Settings.Common.EditableOrganizationConfig exposing (EditableOrganizationConfig)
import Wizard.Settings.Generic.Msgs as GenericMsgs


type alias Msg =
    GenericMsgs.Msg EditableOrganizationConfig
