module Wizard.Admin.Msgs exposing (Msg(..))

import Wizard.Admin.Operations.Msgs


type Msg
    = OperationsMsg Wizard.Admin.Operations.Msgs.Msg
