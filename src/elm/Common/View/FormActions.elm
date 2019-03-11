module Common.View.FormActions exposing (view, viewActionOnly)

import ActionResult exposing (ActionResult(..))
import Common.Html exposing (..)
import Common.View.ActionButton as ActionButton
import Html exposing (..)
import Html.Attributes exposing (..)
import Msgs exposing (Msg)
import Routing exposing (Route)
import String


{-| Helper to show action buttons below the form.

  - Cancel button simply redirects to another route
  - Action button invokes specified message when clicked

-}
view : Route -> ( String, ActionResult a, Msg ) -> Html Msg
view cancelRoute actionButtonSettings =
    div [ class "form-actions" ]
        [ linkTo cancelRoute [ class "btn btn-secondary" ] [ text "Cancel" ]
        , ActionButton.button actionButtonSettings
        ]


{-| Similar to previous, but it contains only the action button.
-}
viewActionOnly : ( String, ActionResult a, msg ) -> Html msg
viewActionOnly actionButtonSettings =
    div [ class "text-right" ]
        [ ActionButton.button actionButtonSettings ]
