port module Ports exposing
    ( FilePortData
    , clearSession
    , createDropzone
    , fileContentRead
    , fileSelected
    , onSessionChange
    , storeSession
    )

import Auth.Models exposing (Session)
import Json.Encode exposing (Value)


port storeSession : Maybe Session -> Cmd msg


port clearSession : () -> Cmd msg


port onSessionChange : (Value -> msg) -> Sub msg


type alias FilePortData =
    { contents : String
    , filename : String
    }


port fileSelected : String -> Cmd msg


port fileContentRead : (FilePortData -> msg) -> Sub msg


port createDropzone : String -> Cmd msg
