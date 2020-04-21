module Shared.Utils exposing (dispatch, nilUuid)

import Task


dispatch : a -> Cmd a
dispatch msg =
    Task.perform (always msg) (Task.succeed ())


nilUuid : String
nilUuid =
    "00000000-0000-0000-0000-000000000000"
