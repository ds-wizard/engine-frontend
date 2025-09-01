module Wizard.Dev.Msgs exposing (Msg(..))

import Wizard.Dev.Operations.Msgs
import Wizard.Dev.PersistentCommandsDetail.Msgs
import Wizard.Dev.PersistentCommandsIndex.Msgs


type Msg
    = OperationsMsg Wizard.Dev.Operations.Msgs.Msg
    | PersistentCommandsDetailMsg Wizard.Dev.PersistentCommandsDetail.Msgs.Msg
    | PersistentCommandsIndexMsg Wizard.Dev.PersistentCommandsIndex.Msgs.Msg
