module DSPlanner.Msgs exposing (Msg(..))

import DSPlanner.Create.Msgs
import DSPlanner.Detail.Msgs
import DSPlanner.Index.Msgs


type Msg
    = CreateMsg DSPlanner.Create.Msgs.Msg
    | DetailMsg DSPlanner.Detail.Msgs.Msg
    | IndexMsg DSPlanner.Index.Msgs.Msg
