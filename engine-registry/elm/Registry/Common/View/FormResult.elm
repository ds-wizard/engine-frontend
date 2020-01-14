module Registry.Common.View.FormResult exposing
    ( errorOnlyView
    , view
    )

import ActionResult exposing (ActionResult(..))
import Html exposing (..)
import Registry.Common.Html exposing (..)
import Registry.Common.View.Flash as Flash
import String


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
