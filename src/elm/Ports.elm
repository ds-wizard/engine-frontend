port module Ports exposing
    ( FilePortData
    , alert
    , clearSession
    , clearUnloadMessage
    , createDropzone
    , fileContentRead
    , fileSelected
    , onSessionChange
    , scrollToTop
    , setUnloadMessage
    , storeSession
    )

import Auth.Models exposing (Session)
import Json.Encode exposing (Value)



-- Session


port storeSession : Maybe Session -> Cmd msg


port clearSession : () -> Cmd msg


port onSessionChange : (Value -> msg) -> Sub msg



-- Import


type alias FilePortData =
    { contents : String
    , filename : String
    }


port fileSelected : String -> Cmd msg


port fileContentRead : (FilePortData -> msg) -> Sub msg


port createDropzone : String -> Cmd msg



-- Scroll


port scrollToTop : String -> Cmd msg



-- Page Unload


port setUnloadMessage : String -> Cmd msg


port clearUnloadMessage : () -> Cmd msg


port alert : String -> Cmd msg
