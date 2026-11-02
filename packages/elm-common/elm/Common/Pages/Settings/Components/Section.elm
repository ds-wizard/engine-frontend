module Common.Pages.Settings.Components.Section exposing
    ( actionResult
    , guard
    , guideLink
    , header
    )

{-| What the settings sections share: the navigation guard's question, the
guide link beside a section's title, and the title row of a section that is not a
`SettingsPage` configuration form.
-}

import ActionResult exposing (ActionResult(..))
import Common.Components.ApiErrorAlert as ApiErrorAlert
import Common.Components.GuideLink as GuideLink
import Common.Data.AppState as AppState exposing (AppState)
import Common.Utils.GuideLinks exposing (GuideLinks)
import Gettext exposing (gettext)
import Html exposing (Html)
import Ui.AsyncBlock as AsyncBlock
import Ui.Button as Button exposing (Button)
import Ui.Layout as Layout
import Ui.Text as Text


{-| The question asked before leaving a section with changes in it.
-}
guard : AppState -> Bool -> Maybe String
guard appState dirty =
    if dirty then
        Just (gettext "There are unsaved changes." appState.locale)

    else
        Nothing


guideLink : AppState -> (GuideLinks -> String) -> Button msg
guideLink appState getLink =
    GuideLink.button appState (AppState.toServerInfo appState) getLink


{-| The section's `h1`, with its actions — a guide link, a create link — beside
it.
-}
header : String -> List (Button msg) -> Html msg
header title actions =
    if List.isEmpty actions then
        Text.pageTitle title

    else
        Layout.inlineBetween
            [ Text.pageTitle title
            , Layout.inline (List.map Button.toHtml actions)
            ]


{-| What goes under a section's title while its data loads, when it failed, and
once it is there. The title stays, so focus on arrival has its `h1`
whatever the request is doing — the same as `SettingsPage.view`.
-}
actionResult : AppState -> (a -> Html msg) -> ActionResult a -> Html msg
actionResult appState viewContent result =
    case result of
        Success content ->
            viewContent content

        Error _ ->
            ApiErrorAlert.view appState result

        _ ->
            AsyncBlock.view
                { loading = True
                , error = Nothing
                , loadingLabel = gettext "Loading..." appState.locale
                , content = []
                }
