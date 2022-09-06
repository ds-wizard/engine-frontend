module Wizard.Projects.Import.Subscriptions exposing (subscriptions)

import Wizard.Ports as Ports
import Wizard.Projects.Import.Msgs exposing (Msg(..))


subscriptions : Sub Msg
subscriptions =
    Ports.gotImporterData GotImporterData
