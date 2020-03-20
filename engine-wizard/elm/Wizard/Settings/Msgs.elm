module Wizard.Settings.Msgs exposing (Msg(..))

import Wizard.Settings.Affiliation.Msgs
import Wizard.Settings.Client.Msgs
import Wizard.Settings.Features.Msgs
import Wizard.Settings.Info.Msgs
import Wizard.Settings.Organization.Msgs


type Msg
    = AffiliationMsg Wizard.Settings.Affiliation.Msgs.Msg
    | ClientMsg Wizard.Settings.Client.Msgs.Msg
    | FeaturesMsg Wizard.Settings.Features.Msgs.Msg
    | InfoMsg Wizard.Settings.Info.Msgs.Msg
    | OrganizationMsg Wizard.Settings.Organization.Msgs.Msg
