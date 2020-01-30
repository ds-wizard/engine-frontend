module Wizard.Documents.Msgs exposing (..)

import Wizard.Documents.Create.Msgs
import Wizard.Documents.Index.Msgs


type Msg
    = CreateMsg Wizard.Documents.Create.Msgs.Msg
    | IndexMsg Wizard.Documents.Index.Msgs.Msg
