module Wizard.Common.Time exposing (getTime, getTimeZone)

import Task
import Time
import Wizard.Msgs exposing (Msg(..))


getTime : Cmd Msg
getTime =
    Task.perform OnTime Time.now


getTimeZone : Cmd Msg
getTimeZone =
    Task.perform OnTimeZone Time.here
