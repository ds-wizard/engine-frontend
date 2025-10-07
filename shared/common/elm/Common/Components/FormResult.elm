module Common.Components.FormResult exposing
    ( errorOnlyView
    , successOnlyView
    , view
    )

import ActionResult exposing (ActionResult(..))
import Common.Components.Flash as Flash
import Html exposing (Html)
import Html.Extra as Html


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
