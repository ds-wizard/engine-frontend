module Common.Pages.Users.Components.Section exposing
    ( actionResult
    , focusHeading
    , subsection
    , view
    )

{-| The body of one user settings tab, and the pieces the tabs share.

Every tab starts with its `h1` whatever state its data is in, because after a
route change focus lands on the page heading, and a tab that is still loading or
failed to load has to have one to land on.

-}

import ActionResult exposing (ActionResult(..))
import Browser.Dom
import Common.Components.ApiErrorAlert as ApiErrorAlert
import Common.Components.GuideLink as GuideLink
import Common.Data.AppState as AppState exposing (AppState)
import Common.Utils.GuideLinks exposing (GuideLinks)
import Gettext exposing (gettext)
import Html exposing (Html)
import Task
import Ui.AppShell as AppShell
import Ui.AsyncBlock as AsyncBlock
import Ui.Button as Button
import Ui.Layout as Layout
import Ui.Separator as Separator
import Ui.Text as Text


{-| The tab's title (the page's `h1`), its guide link beside it, and the
content under them.
-}
view : AppState -> { title : String, guideLink : Maybe (GuideLinks -> String), content : List (Html msg) } -> List (Html msg)
view appState config =
    let
        header =
            case config.guideLink of
                Just guideLink ->
                    Layout.inlineBetween
                        [ Text.pageTitle config.title
                        , Button.toHtml (GuideLink.button appState (AppState.toServerInfo appState) guideLink)
                        ]

                Nothing ->
                    Text.pageTitle config.title
    in
    [ Layout.stack (header :: config.content) ]


{-| A second subject on the same tab — the keys under the form that makes one,
the accounts under the button that connects one. A rule across the page, then
the `h2` naming it: without the rule the heading reads as a caption for what is
above it rather than the start of something else.
-}
subsection : String -> List (Html msg) -> List (Html msg)
subsection title content =
    Separator.line :: Text.title title :: content


{-| The loading state, the error, or the content once it is there. The heading
stays above all three.
-}
actionResult : AppState -> (a -> List (Html msg)) -> ActionResult a -> List (Html msg)
actionResult appState viewContent result =
    case result of
        Unset ->
            []

        Loading ->
            [ AsyncBlock.view
                { loading = True
                , error = Nothing
                , loadingLabel = gettext "Loading..." appState.locale
                , content = []
                }
            ]

        Error _ ->
            [ ApiErrorAlert.view appState result ]

        Success data ->
            viewContent data


{-| After the row that had focus is removed, put focus on the page heading.
-}
focusHeading : msg -> Cmd msg
focusHeading msg =
    Browser.Dom.focus AppShell.mainHeadingId
        |> Task.attempt (always msg)
