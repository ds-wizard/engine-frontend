module Common.View.FormActions exposing (view, viewActionOnly)

import Common.Html exposing (..)
import Common.View.ActionButton as ActionButton
import Html exposing (..)
import Html.Attributes exposing (..)
import Routing exposing (Route)


{-| Helper to show action buttons below the form.

  - Cancel button simply redirects to another route
  - Action button invokes specified message when clicked

-}
view : Route -> ActionButton.ButtonConfig a msg -> Html msg
view cancelRoute actionButtonConfig =
    div [ class "form-actions" ]
        [ linkTo cancelRoute [ class "btn btn-secondary" ] [ text "Cancel" ]
        , ActionButton.button actionButtonConfig
        ]


{-| Similar to previous, but it contains only the action button.
-}
viewActionOnly : ActionButton.ButtonConfig a msg -> Html msg
viewActionOnly actionButtonConfig =
    div [ class "text-right" ]
        [ ActionButton.button actionButtonConfig ]
