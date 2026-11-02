module Common.Pages.Dashboard.Widgets.WidgetHelpers exposing (asyncBody)

{-| What the dashboard's widgets share.
-}

import ActionResult exposing (ActionResult(..))
import Common.Data.AppState exposing (AppState)
import Gettext exposing (gettext)
import Html exposing (Html)
import Ui.AsyncBlock as AsyncBlock


{-| A widget body that loads: a polite loading status, an error alert, or the
content. Nothing has been asked for yet is the same as loading, because every
widget fetches on the dashboard's `init`.
-}
asyncBody : AppState -> (a -> List (Html msg)) -> ActionResult a -> Html msg
asyncBody appState viewContent result =
    let
        ( loading, error, content ) =
            case result of
                Success data ->
                    ( False, Nothing, viewContent data )

                Error message ->
                    ( False, Just message, [] )

                _ ->
                    ( True, Nothing, [] )
    in
    AsyncBlock.view
        { loading = loading
        , error = error
        , loadingLabel = gettext "Loading..." appState.locale
        , content = content
        }
