port module Common.Ports.SideNav exposing (uiStoreSideNav)

{-| Where the width someone dragged the navigation to is kept.

`Ui.SideNav` has no port of its own; `packages/elm-ui/js/side-nav-storage.js` is the
shared reader and writer behind this one, and `index.js` reads the stored width
back into the flags before the first paint. The collapsed flag is written too,
because the storage keeps both, but this application reads it from the session, where the wizard has always kept it.

-}


port uiStoreSideNav : { collapsed : Bool, width : Float } -> Cmd msg
