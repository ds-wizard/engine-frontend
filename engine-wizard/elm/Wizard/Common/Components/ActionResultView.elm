module Wizard.Common.Components.ActionResultView exposing (error)

import ActionResult exposing (ActionResult(..))
import Html exposing (Html, span, text)
import Html.Attributes exposing (class)
import Shared.Html exposing (emptyNode)


error : ActionResult a -> Html msg
error actionResult =
    case actionResult of
        Error err ->
            span [ class "text-danger action-result-error" ] [ text err ]

        _ ->
            emptyNode
