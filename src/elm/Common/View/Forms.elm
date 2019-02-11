module Common.View.Forms exposing
    ( formActionOnly
    , formActions
    )

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
