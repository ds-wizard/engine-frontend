port module Wizard.Ports.Import exposing
    ( createDropzone
    , fileContentRead
    , fileSelected
    )

import Json.Encode as E


port fileSelected : String -> Cmd msg


port fileContentRead : (E.Value -> msg) -> Sub msg


port createDropzone : String -> Cmd msg
