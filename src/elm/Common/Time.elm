module Common.Time exposing (getTime)

import Msgs exposing (Msg(..))
import Task
import Time


getTime : Cmd Msg
getTime =
    Task.perform OnTime Time.now
