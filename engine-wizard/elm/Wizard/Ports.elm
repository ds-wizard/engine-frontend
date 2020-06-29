port module Wizard.Ports exposing
    ( alert
    , clearSession
    , clearSessionAndReload
    , clearUnloadMessage
    , copyToClipboard
    , createDropzone
    , drawMetricsChart
    , fileContentRead
    , fileSelected
    , refresh
    , scrollIntoView
    , scrollToTop
    , setUnloadMessage
    , storeSession
    )

import Json.Encode as E exposing (Value)



-- Session


port storeSession : E.Value -> Cmd msg


port clearSession : () -> Cmd msg


port clearSessionAndReload : () -> Cmd msg



-- Import


port fileSelected : String -> Cmd msg


port fileContentRead : (E.Value -> msg) -> Sub msg


port createDropzone : String -> Cmd msg



-- Scroll


port scrollIntoView : String -> Cmd msg


port scrollToTop : String -> Cmd msg



-- Page Unload


port setUnloadMessage : String -> Cmd msg


port clearUnloadMessage : () -> Cmd msg


port alert : String -> Cmd msg



-- Refresh


port refresh : () -> Cmd msg



-- Charts


port drawMetricsChart : E.Value -> Cmd msg



-- Copy


port copyToClipboard : String -> Cmd msg
