port module Wizard.Ports exposing
    ( acceptCookies
    , alert
    , clearSession
    , clearSessionAndReload
    , clearUnloadMessage
    , consoleError
    , createDropzone
    , createLocaleDropzone
    , downloadFile
    , fileContentRead
    , fileSelected
    , focus
    , gotScrollTop
    , historyBack
    , historyBackCallback
    , localStorageData
    , localStorageGet
    , localStorageGetAndRemove
    , localStorageSet
    , localeFileSelected
    , refresh
    , scrollIntoView
    , scrollIntoViewCenter
    , scrollToTop
    , setScrollTop
    , setUnloadMessage
    , storeSession
    , subscribeScrollTop
    )

import Json.Encode as E
import Wizard.Common.ElementScrollTop as ElementScrollTop exposing (ElementScrollTop)



-- Browser


port historyBack : String -> Cmd msg


port historyBackCallback : (String -> msg) -> Sub msg



-- Console


port consoleError : String -> Cmd msg



-- Session


port storeSession : E.Value -> Cmd msg


port clearSession : () -> Cmd msg


port clearSessionAndReload : () -> Cmd msg



-- Import


port fileSelected : String -> Cmd msg


port localeFileSelected : String -> Cmd msg


port fileContentRead : (E.Value -> msg) -> Sub msg


port createDropzone : String -> Cmd msg


port createLocaleDropzone : String -> Cmd msg



-- DOM


port focus : String -> Cmd msg


port scrollIntoView : String -> Cmd msg


port scrollIntoViewCenter : String -> Cmd msg


port scrollToTop : String -> Cmd msg


setScrollTop : ElementScrollTop -> Cmd msg
setScrollTop =
    setScrollTopPort << ElementScrollTop.encode


port setScrollTopPort : E.Value -> Cmd msg


port subscribeScrollTop : String -> Cmd msg


port gotScrollTop : (E.Value -> msg) -> Sub msg



-- Page Unload


port setUnloadMessage : String -> Cmd msg


port clearUnloadMessage : () -> Cmd msg


port alert : String -> Cmd msg



-- Refresh


port refresh : () -> Cmd msg



-- Cookies


port acceptCookies : () -> Cmd msg



-- File Download


port downloadFile : String -> Cmd msg



-- Local Storage


port localStorageGet : String -> Cmd msg


port localStorageGetAndRemove : String -> Cmd msg


port localStorageSet : E.Value -> Cmd msg


port localStorageData : (E.Value -> msg) -> Sub msg
