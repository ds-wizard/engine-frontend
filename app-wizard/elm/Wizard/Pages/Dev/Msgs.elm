module Wizard.Pages.Dev.Msgs exposing (Msg(..))

import Wizard.Pages.Dev.Operations.Msgs
import Wizard.Pages.Dev.PersistentCommandsDetail.Msgs
import Wizard.Pages.Dev.PersistentCommandsIndex.Msgs


type Msg
    = OperationsMsg Wizard.Pages.Dev.Operations.Msgs.Msg
    | PersistentCommandsDetailMsg Wizard.Pages.Dev.PersistentCommandsDetail.Msgs.Msg
    | PersistentCommandsIndexMsg Wizard.Pages.Dev.PersistentCommandsIndex.Msgs.Msg
