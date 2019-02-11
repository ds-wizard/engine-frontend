module Common.View.Forms exposing
    ( formActionOnly
    , formActions
    , formErrorResultView
    , formResultView
    , formSuccessResultView
    )

import ActionResult exposing (ActionResult(..))
import Common.Html exposing (..)
import Common.View.ActionButton as ActionButton
import Common.View.Flash as Flash
import Html exposing (..)
import Html.Attributes exposing (..)
import Msgs exposing (Msg)
import Routing exposing (Route)
import String


{-| Helper to show action buttons below the form.

  - Cancel button simply redirects to another route
  - Action button invokes specified message when clicked

-}
formActions : Route -> ( String, ActionResult a, Msg ) -> Html Msg
formActions cancelRoute actionButtonSettings =
    div [ class "form-actions" ]
        [ linkTo cancelRoute [ class "btn btn-secondary" ] [ text "Cancel" ]
        , ActionButton.button actionButtonSettings
        ]


{-| Similar to formActions, but it contains only the action button.
-}
formActionOnly : ( String, ActionResult a, msg ) -> Html msg
formActionOnly actionButtonSettings =
    div [ class "text-right" ]
        [ ActionButton.button actionButtonSettings ]


formResultView : ActionResult String -> Html msg
formResultView result =
    case result of
        Success msg ->
            Flash.success msg

        Error msg ->
            Flash.error msg

        _ ->
            emptyNode


formSuccessResultView : ActionResult String -> Html msg
formSuccessResultView result =
    case result of
        Success msg ->
            Flash.success msg

        _ ->
            emptyNode


formErrorResultView : ActionResult String -> Html msg
formErrorResultView result =
    case result of
        Error msg ->
            Flash.error msg

        _ ->
            emptyNode
