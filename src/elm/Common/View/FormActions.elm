module Common.View.FormActions exposing (view, viewActionOnly)

import Common.AppState exposing (AppState)
import Common.Html exposing (..)
import Common.Locale exposing (lx)
import Common.View.ActionButton as ActionButton
import Html exposing (..)
import Html.Attributes exposing (..)
import Routes


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Common.View.FormActions"


{-| Helper to show action buttons below the form.

  - Cancel button simply redirects to another route
  - Action button invokes specified message when clicked

-}
view : AppState -> Routes.Route -> ActionButton.ButtonConfig a msg -> Html msg
view appState cancelRoute actionButtonConfig =
    div [ class "form-actions" ]
        [ linkTo appState cancelRoute [ class "btn btn-secondary" ] [ lx_ "cancelButton.cancel" appState ]
        , ActionButton.button actionButtonConfig
        ]


{-| Similar to previous, but it contains only the action button.
-}
viewActionOnly : ActionButton.ButtonConfig a msg -> Html msg
viewActionOnly actionButtonConfig =
    div [ class "text-right" ]
        [ ActionButton.button actionButtonConfig ]
