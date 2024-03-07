module Registry.Components.FormResult exposing
    ( errorOnlyView
    , view
    )

import ActionResult exposing (ActionResult(..))
import Html exposing (Html)
import Registry.Components.Flash as Flash
import Shared.Html exposing (emptyNode)


view : ActionResult String -> Html msg
view result =
    case result of
        Success msg ->
            Flash.success msg

        Error msg ->
            Flash.error msg

        _ ->
            emptyNode


errorOnlyView : ActionResult a -> Html msg
errorOnlyView result =
    case result of
        Error msg ->
            Flash.error msg

        _ ->
            emptyNode
