module Wizard.Common.View.FormResult exposing
    ( errorOnlyView
    , successOnlyView
    , view
    )

import ActionResult exposing (ActionResult(..))
import Html exposing (Html)
import Shared.Html exposing (emptyNode)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.View.Flash as Flash


view : AppState -> ActionResult String -> Html msg
view appState result =
    case result of
        Success msg ->
            Flash.success appState msg

        Error msg ->
            Flash.error appState msg

        _ ->
            emptyNode


successOnlyView : AppState -> ActionResult String -> Html msg
successOnlyView appState result =
    case result of
        Success msg ->
            Flash.success appState msg

        _ ->
            emptyNode


errorOnlyView : AppState -> ActionResult a -> Html msg
errorOnlyView appState result =
    case result of
        Error msg ->
            Flash.error appState msg

        _ ->
            emptyNode
