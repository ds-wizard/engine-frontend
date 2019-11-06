module Wizard.Common.Time exposing (getTime)

import Task
import Time
import Wizard.Msgs exposing (Msg(..))


getTime : Cmd Msg
getTime =
    Task.perform OnTime Time.now
