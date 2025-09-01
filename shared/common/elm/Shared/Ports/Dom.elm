port module Shared.Ports.Dom exposing
    ( focus
    , gotScrollTop
    , scrollIntoView
    , scrollIntoViewCenter
    , scrollIntoViewInstant
    , scrollToTop
    , scrollTreeItemIntoView
    , setScrollTop
    , subscribeScrollTop
    )

import Json.Encode as E
import Shared.Ports.Dom.ElementScrollTop as ElementScrollTop exposing (ElementScrollTop)


port focus : String -> Cmd msg


port scrollIntoView : String -> Cmd msg


port scrollIntoViewInstant : String -> Cmd msg


port scrollIntoViewCenter : String -> Cmd msg


port scrollTreeItemIntoView : String -> Cmd msg


port scrollToTop : String -> Cmd msg


setScrollTop : ElementScrollTop -> Cmd msg
setScrollTop =
    setScrollTopPort << ElementScrollTop.encode


port setScrollTopPort : E.Value -> Cmd msg


port subscribeScrollTop : String -> Cmd msg


port gotScrollTop : (E.Value -> msg) -> Sub msg
