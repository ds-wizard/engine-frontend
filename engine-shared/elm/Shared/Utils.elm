module Shared.Utils exposing (dispatch)

import Task


dispatch : a -> Cmd a
dispatch msg =
    Task.perform (always msg) (Task.succeed ())
