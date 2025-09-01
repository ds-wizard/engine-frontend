module Shared.Components.FormResult exposing
    ( errorOnlyView
    , successOnlyView
    , view
    )

import ActionResult exposing (ActionResult(..))
import Html exposing (Html)
import Html.Extra as Html
import Shared.Components.Flash as Flash


view : ActionResult String -> Html msg
view result =
    case result of
        Success msg ->
            Flash.success msg

        Error msg ->
            Flash.error msg

        _ ->
            Html.nothing


successOnlyView : ActionResult String -> Html msg
successOnlyView result =
    case result of
        Success msg ->
            Flash.success msg

        _ ->
            Html.nothing


errorOnlyView : ActionResult a -> Html msg
errorOnlyView result =
    case result of
        Error msg ->
            Flash.error msg

        _ ->
            Html.nothing
