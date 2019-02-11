module Common.View.FormResult exposing
    ( errorOnlyView
    , successOnlyView
    , view
    )

import ActionResult exposing (ActionResult(..))
import Common.Html exposing (..)
import Common.View.Flash as Flash
import Html exposing (..)
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


successOnlyView : ActionResult String -> Html msg
successOnlyView result =
    case result of
        Success msg ->
            Flash.success msg

        _ ->
            emptyNode


errorOnlyView : ActionResult String -> Html msg
errorOnlyView result =
    case result of
        Error msg ->
            Flash.error msg

        _ ->
            emptyNode
