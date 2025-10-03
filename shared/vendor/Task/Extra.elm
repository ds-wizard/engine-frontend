module Task.Extra exposing (dispatch, dispatchAfter)

import Process
import Task


dispatch : a -> Cmd a
dispatch msg =
    Task.perform (always msg) (Task.succeed ())


dispatchAfter : Float -> a -> Cmd a
dispatchAfter time msg =
    Process.sleep time
        |> Task.andThen (\_ -> Task.succeed msg)
        |> Task.perform identity
