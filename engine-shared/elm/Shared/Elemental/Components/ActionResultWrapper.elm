module Shared.Elemental.Components.ActionResultWrapper exposing (blockLG, blockSM, page)

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


blockSM : Theme -> (a -> Html msg) -> ActionResult a -> Html msg
blockSM theme viewContent actionResult =
    case actionResult of
        Unset ->
            emptyNode

        Loading ->
            Loader.blockSM theme

        Error err ->
            div [] [ text <| "error: " ++ err ]

        Success data ->
            viewContent data


blockLG : Theme -> (a -> Html msg) -> ActionResult a -> Html msg
blockLG theme viewContent actionResult =
    case actionResult of
        Unset ->
            emptyNode

        Loading ->
            Loader.blockLG theme

        Error err ->
            div [] [ text <| "error: " ++ err ]

        Success data ->
            viewContent data
