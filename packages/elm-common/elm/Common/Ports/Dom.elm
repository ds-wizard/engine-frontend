port module Common.Ports.Dom exposing
    ( ScrollTop
    , focus
    , focusAndSetCaret
    , focusWithCaret
    , gotScrollTop
    , scrollToAndFocus
    , setScrollTop
    , watchScrollTop
    )

{-| Moving focus and scrolling. See `packages/elm-common/js/ports/dom.js`.

`focus` and `focusAndSetCaret` do one thing each, at once. The rest wait for the
next frame and combine a scroll with a focus, so they find what the render that
follows the command put on the screen rather than what it replaced.

-}

import Json.Decode as D
import Json.Encode as E


{-| Move focus to the first element matching the selector.
-}
port focus : String -> Cmd msg


{-| Focus a text box and put the caret at `caretPos`, in one step. When the box's
value arrives in the same render, use `focusWithCaret` instead: setting a
textarea's value moves the caret to the end, undoing a caret set before it.
-}
focusAndSetCaret : String -> Int -> Cmd msg
focusAndSetCaret elementId caretPos =
    focusAndSetCaretPort <|
        E.object
            [ ( "elementSelector", E.string elementId )
            , ( "caretPos", E.int caretPos )
            ]


port focusAndSetCaretPort : E.Value -> Cmd msg


{-| Scroll `selector` into view — smoothly, or at once for a deep link — and move
focus to `focus` without a second, instant scroll of its own. The target of the
focus should carry `tabindex="-1"` if it is not a control.

Both wait for the next frame first, so they find what the render that follows the
command put on the screen (a new chapter's question), not what it replaced.

The focus is ringed after a key press only. After a click the scroll says where
the reader landed, and a ring would be one more thing on the screen.

-}
scrollToAndFocus : { selector : String, focus : String, smooth : Bool } -> Cmd msg
scrollToAndFocus config =
    commonScrollToAndFocus
        (E.object
            [ ( "selector", E.string config.selector )
            , ( "focus", E.string config.focus )
            , ( "smooth", E.bool config.smooth )
            ]
        )


{-| Put focus in a text box and the caret at `caret`.

After the render, not before it: a command runs before the render that follows
it, and setting a textarea's value puts its caret at the end — so a caret set
first is undone by the value arriving second. `focusAndSetCaret` above sets it at once, which is why this one exists.

For a box something writes into: a template with values under it, each of which
inserts itself and leaves the caret after what it wrote.

-}
focusWithCaret : { selector : String, caret : Int } -> Cmd msg
focusWithCaret config =
    commonFocusWithCaret
        (E.object
            [ ( "selector", E.string config.selector )
            , ( "caret", E.int config.caret )
            ]
        )


type alias ScrollTop =
    { selector : String
    , scrollTop : Int
    }


{-| Report where `selector` is scrolled to, once each time the scrolling settles —
not on every scroll event, which would be an update and a render per frame for a
number nobody reads until the reader comes back.
-}
watchScrollTop : String -> Cmd msg
watchScrollTop =
    commonWatchScrollTop


gotScrollTop : (ScrollTop -> msg) -> Sub msg
gotScrollTop toMsg =
    commonGotScrollTop
        (\value ->
            toMsg
                (Result.withDefault { selector = "", scrollTop = 0 }
                    (D.decodeValue
                        (D.map2 ScrollTop (D.field "selector" D.string) (D.field "scrollTop" D.int))
                        value
                    )
                )
        )


{-| Scroll `selector` to `scrollTop`, once it is on the screen.
-}
setScrollTop : ScrollTop -> Cmd msg
setScrollTop config =
    commonSetScrollTop
        (E.object
            [ ( "selector", E.string config.selector )
            , ( "scrollTop", E.int config.scrollTop )
            ]
        )


port commonScrollToAndFocus : E.Value -> Cmd msg


port commonFocusWithCaret : E.Value -> Cmd msg


port commonWatchScrollTop : String -> Cmd msg


port commonGotScrollTop : (E.Value -> msg) -> Sub msg


port commonSetScrollTop : E.Value -> Cmd msg
