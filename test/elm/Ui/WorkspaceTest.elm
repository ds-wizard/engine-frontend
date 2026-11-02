module Ui.WorkspaceTest exposing (suite)

import Html
import Html.Attributes as Attr
import Json.Encode
import Test exposing (Test, describe, test)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, class, id, tag, text)
import Ui.Page.Workspace as Workspace
import Ui.Resizer as Resizer


type Msg
    = OverlayOpened
    | OverlayClosed
    | ClosePanel
    | Resize


words : String -> Resizer.Words
words label =
    { label = label, collapsed = "Collapsed", width = String.fromInt }


withNav : Bool -> Workspace.Workspace Msg -> Workspace.Workspace Msg
withNav open =
    Workspace.withNav
        { label = "Chapters"
        , header = []
        , content = [ Html.text "the chapters" ]
        , resizer = Resizer.init { edge = Resizer.Trailing, label = "Resize", min = 100, max = 300, default = 200, collapse = Nothing }
        , toResizerMsg = always Resize
        , resizerWords = words "Resize chapters"
        , overlayOpen = open
        , onOverlay =
            \open_ ->
                if open_ then
                    OverlayOpened

                else
                    OverlayClosed
        , toggleLabel = "Chapters"
        , closeLabel = "Close chapters"
        }


withAside : Workspace.Workspace Msg -> Workspace.Workspace Msg
withAside =
    withAsideClosing False


withAsideClosing : Bool -> Workspace.Workspace Msg -> Workspace.Workspace Msg
withAsideClosing closeInPanel =
    Workspace.withAside
        { title = "Search"
        , content = [ Html.text "results" ]
        , resizer = Resizer.init { edge = Resizer.Leading, label = "Resize", min = 100, max = 300, default = 200, collapse = Nothing }
        , toResizerMsg = always Resize
        , resizerWords = words "Resize panel"
        , onClose = ClosePanel
        , closeLabel = "Close panel"
        , closeInPanel = closeInPanel
        }


render : Workspace.Workspace Msg -> Query.Single Msg
render workspace =
    Query.fromHtml (Workspace.toHtml workspace)


base : Workspace.Workspace Msg
base =
    Workspace.workspace { id = "ws", main = [ Html.text "the work" ] }


{-| What the workspace promises the app and assistive technology: ids made from
its own, the narrow navigation as a disclosure the toolbar controls, and the panel
as a complementary region named by its heading.
-}
suite : Test
suite =
    describe "Ui.Page.Workspace"
        [ test "the work's scroll container has the id mainId gives" <|
            \_ ->
                render base
                    |> Query.find [ id (Workspace.mainId "ws") ]
                    |> Query.has [ text "the work" ]
        , test "the navigation is a named nav landmark with navId" <|
            \_ ->
                render (withNav False base)
                    |> Query.find [ tag "nav" ]
                    |> Query.has [ id (Workspace.navId "ws"), attribute (Attr.attribute "aria-label" "Chapters") ]
        , test "the toggle controls the navigation and says whether it is open" <|
            \_ ->
                render (withNav True base)
                    |> Query.find [ id (Workspace.navToggleId "ws") ]
                    |> Query.has
                        [ attribute (Attr.attribute "aria-controls" (Workspace.navId "ws"))
                        , attribute (Attr.attribute "aria-expanded" "true")
                        ]
        , test "the toggle asks for the opposite of what is showing" <|
            \_ ->
                render (withNav False base)
                    |> Query.find [ id (Workspace.navToggleId "ws") ]
                    |> Event.simulate Event.click
                    |> Event.expect OverlayOpened
        , test "without a toolbar of the app's, the toggle has a row of its own" <|
            \_ ->
                render (withNav False base)
                    |> Query.has [ class "ui-workspace__toolbar--nav-only" ]
        , test "the panel is an aside labeled by its heading" <|
            \_ ->
                render (withAside base)
                    |> Query.find [ tag "aside" ]
                    |> Query.has [ attribute (Attr.attribute "aria-labelledby" (Workspace.asideId "ws" ++ "-title")) ]
        , test "the panel's close button closes it" <|
            \_ ->
                render (withAside base)
                    |> Query.find [ tag "aside" ]
                    |> Query.find [ attribute (Attr.attribute "aria-label" "Close panel") ]
                    |> Event.simulate Event.click
                    |> Event.expect ClosePanel
        , test "a panel opened from the toolbar leaves its close button to the narrow layout" <|
            \_ ->
                render (withAside base)
                    |> Query.find [ class "ui-workspace__aside-close" ]
                    |> Query.hasNot [ class "is-always-shown" ]
        , test "a panel opened from inside the work draws its close button at every width" <|
            \_ ->
                render (withAsideClosing True base)
                    |> Query.find [ class "ui-workspace__aside-close" ]
                    |> Query.has [ class "is-always-shown" ]
        , test "the work can be told to take the whole width" <|
            \_ ->
                render (Workspace.withFlushMain base)
                    |> Query.find [ id (Workspace.mainId "ws") ]
                    |> Query.has [ class "is-flush" ]
        , test "Escape in the open navigation closes it" <|
            \_ ->
                render (withNav True base)
                    |> Query.find [ tag "nav" ]
                    |> Event.simulate (Event.custom "keydown" (Json.Encode.object [ ( "key", Json.Encode.string "Escape" ) ]))
                    |> Event.expect OverlayClosed
        ]
