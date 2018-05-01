module Questionnaires.Msgs exposing (..)

import Questionnaires.Create.Msgs
import Questionnaires.Detail.Msgs
import Questionnaires.Index.Msgs


type Msg
    = CreateMsg Questionnaires.Create.Msgs.Msg
    | DetailMsg Questionnaires.Detail.Msgs.Msg
    | IndexMsg Questionnaires.Index.Msgs.Msg
