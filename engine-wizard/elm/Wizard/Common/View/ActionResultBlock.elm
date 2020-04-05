module Wizard.Common.View.ActionResultBlock exposing (view)

import ActionResult exposing (ActionResult(..))
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Shared.Html exposing (emptyNode)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.View.Page as Page


view : AppState -> (a -> Html msg) -> ActionResult a -> Html msg
view appState viewContent actionResult =
    case actionResult of
        Unset ->
            emptyNode

        Loading ->
            Page.loader appState

        Error err ->
            div [ class "alert alert-danger" ] [ text err ]

        Success result ->
            viewContent result
