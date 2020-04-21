module Shared.Elemental.Components.ActionResultWrapper exposing (block, page)

import ActionResult exposing (ActionResult(..))
import Html.Styled exposing (Html, div, text)
import Shared.Elemental.Components.Loader as Loader
import Shared.Elemental.Theme exposing (Theme)
import Shared.Html.Styled exposing (emptyNode)


page : Theme -> (a -> Html msg) -> ActionResult a -> Html msg
page theme viewContent actionResult =
    case actionResult of
        Unset ->
            emptyNode

        Loading ->
            Loader.page theme

        Error err ->
            div [] [ text <| "error: " ++ err ]

        Success data ->
            viewContent data


block : Theme -> (a -> Html msg) -> ActionResult a -> Html msg
block theme viewContent actionResult =
    case actionResult of
        Unset ->
            emptyNode

        Loading ->
            Loader.block theme

        Error err ->
            div [] [ text <| "error: " ++ err ]

        Success data ->
            viewContent data
