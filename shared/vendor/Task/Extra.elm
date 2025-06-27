module Task.Extra exposing (dispatch)

import Task


dispatch : a -> Cmd a
dispatch msg =
    Task.perform (always msg) (Task.succeed ())
