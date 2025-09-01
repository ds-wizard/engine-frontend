module Wizard.Components.ActionResultView exposing (error)

import ActionResult exposing (ActionResult(..))
import Html exposing (Html, span, text)
import Html.Attributes exposing (class)
import Html.Extra as Html


error : ActionResult a -> Html msg
error actionResult =
    case actionResult of
        Error err ->
            span [ class "text-danger mx-4" ] [ text err ]

        _ ->
            Html.nothing
