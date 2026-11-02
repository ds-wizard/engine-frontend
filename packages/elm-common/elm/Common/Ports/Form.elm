port module Common.Ports.Form exposing (cancelFocusFirstField, focusFirstFieldIn)

{-| Moving focus to the first field of a form that has just opened.

A port because the question is about the rendered page, not the model: which
field is first is decided by the document order of whatever the view happened to
render, and `Browser.Dom` can focus an id but cannot find the first input inside
a form. See
`packages/elm-common/js/ports/form.js`.

-}


{-| Focus the first field inside the element with this id, once it is there. It
waits for a form that is still loading, and gives up after a few seconds.
-}
focusFirstFieldIn : String -> Cmd msg
focusFirstFieldIn containerId =
    commonFormFocusFirstField (Just containerId)


{-| Drop a `focusFirstFieldIn` still waiting for its form, so a page left before
it loaded does not pull focus back to itself.
-}
cancelFocusFirstField : Cmd msg
cancelFocusFirstField =
    commonFormFocusFirstField Nothing


port commonFormFocusFirstField : Maybe String -> Cmd msg
