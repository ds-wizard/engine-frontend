module Questionnaires.Msgs exposing (Msg(..))

import Questionnaires.Create.Msgs
import Questionnaires.Detail.Msgs
import Questionnaires.Edit.Msgs
import Questionnaires.Index.Msgs


type Msg
    = CreateMsg Questionnaires.Create.Msgs.Msg
    | DetailMsg Questionnaires.Detail.Msgs.Msg
    | EditMsg Questionnaires.Edit.Msgs.Msg
    | IndexMsg Questionnaires.Index.Msgs.Msg
