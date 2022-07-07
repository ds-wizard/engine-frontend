port module Wizard.Ports exposing
    ( acceptCookies
    , alert
    , clearSession
    , clearSessionAndReload
    , clearUnloadMessage
    , consoleError
    , createDropzone
    , downloadFile
    , fileContentRead
    , fileSelected
    , gotIntegrationWidgetValue
    , openIntegrationWidget
    , refresh
    , scrollIntoView
    , scrollToTop
    , setUnloadMessage
    , storeSession
    )

import Json.Encode as E



-- Console


port consoleError : String -> Cmd msg



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



-- Cookies


port acceptCookies : () -> Cmd msg



-- Integration Widget


port openIntegrationWidget : E.Value -> Cmd msg


port gotIntegrationWidgetValue : (E.Value -> msg) -> Sub msg



-- File Download


port downloadFile : String -> Cmd msg
