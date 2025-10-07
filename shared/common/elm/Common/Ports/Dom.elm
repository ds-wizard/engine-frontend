port module Common.Ports.Dom exposing
    ( focus
    , focusAndSetCaret
    , gotScrollTop
    , scrollIntoView
    , scrollIntoViewCenter
    , scrollIntoViewInstant
    , scrollToTop
    , scrollTreeItemIntoView
    , setScrollTop
    , subscribeScrollTop
    )

import Common.Ports.Dom.ElementScrollTop as ElementScrollTop exposing (ElementScrollTop)
import Json.Encode as E


port focus : String -> Cmd msg


focusAndSetCaret : String -> Int -> Cmd msg
focusAndSetCaret elementId caretPos =
    focusAndSetCaretPort <|
        E.object
            [ ( "elementSelector", E.string elementId )
            , ( "caretPos", E.int caretPos )
            ]


port focusAndSetCaretPort : E.Value -> Cmd msg


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
