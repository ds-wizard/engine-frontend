module Wizard.Settings.Affiliation.Msgs exposing (Msg)

import Wizard.Settings.Common.EditableAffiliationConfig exposing (EditableAffiliationConfig)
import Wizard.Settings.Generic.Msgs as GenericMsgs


type alias Msg =
    GenericMsgs.Msg EditableAffiliationConfig
