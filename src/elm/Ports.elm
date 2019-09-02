port module Ports exposing
    ( FilePortData
    , alert
    , clearSession
    , clearUnloadMessage
    , copyToClipboard
    , createDropzone
    , drawMetricsChart
    , fileContentRead
    , fileSelected
    , scrollIntoView
    , scrollToTop
    , setUnloadMessage
    , storeSession
    )

import Auth.Models exposing (Session)
import Json.Encode as Encode exposing (Value)



-- Session


port storeSession : Maybe Session -> Cmd msg


port clearSession : () -> Cmd msg



-- Import


type alias FilePortData =
    { contents : String
    , filename : String
    }


port fileSelected : String -> Cmd msg


port fileContentRead : (FilePortData -> msg) -> Sub msg


port createDropzone : String -> Cmd msg



-- Scroll


port scrollIntoView : String -> Cmd msg


port scrollToTop : String -> Cmd msg



-- Page Unload


port setUnloadMessage : String -> Cmd msg


port clearUnloadMessage : () -> Cmd msg


port alert : String -> Cmd msg



-- Charts


port drawMetricsChart : Encode.Value -> Cmd msg



-- Copy


port copyToClipboard : String -> Cmd msg
